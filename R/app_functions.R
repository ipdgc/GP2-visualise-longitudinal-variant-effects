#' Get logos
#'
#' This function will print IPDGC and GP2 logos to the web page.
#'
#' @return IPDGC and GP2 logos
#' 

get_logos <- function(){
  
  shiny::column(
    width = 12, 
    shiny::br(),
    shiny::hr(),
    shiny::p(
      shiny::img(src="ipdgc_logo.png", width="20%"), 
      shiny::img(src="gp2_logo.png", width="20%"),
      align="center"
    )
  )
  
}

#' Get toy data set.
#'
#' This function will create a merged dataframe from the \code{test_data}
#' directory.
#'
#' @return

get_toy_data <- function(){
  
  # Create dataframe of files for loading
  file_df <-
    tibble::tibble(
      full_file_path =
        list.files(
          path = here::here("test_data", "cohort"),
          recursive = T,
          full.names = T
        ),
      short_file_path =
        list.files(
          path = here::here("test_data", "cohort"),
          recursive = T,
          full.names = F
        )
    ) %>%
    dplyr::mutate(
      cohort =
        short_file_path %>% 
        stringr::str_remove("/.*"),
      data_type =
        short_file_path %>% 
        stringr::str_remove(
          stringr::str_c(cohort, "/result/")
        ) %>% 
        stringr::str_remove("/.*"),
      biomarker =
        short_file_path %>% 
        stringr::str_remove(
          stringr::str_c(cohort, "/result/", data_type, "/")
        ) %>% 
        stringr::str_remove("/.*"),
      list_name = 
        stringr::str_c(
          cohort, data_type, biomarker,
          sep = ":"
        )
    )
  
  # Create empty list of length nrow(file_df)
  data_list <- 
    setNames(
      vector(
        mode = "list", 
        length = nrow(file_df)
      ),
      nm = file_df$list_name
    )
  
  # Load data
  for(i in 1:nrow(file_df)){
    
    data_type <- file_df$data_type[i]
    
    # Need an if/else statement to deal with the different data types
    if(data_type == "cs"){
      
      data_list[[i]] <- 
        readr::read_delim(
          file =
            file_df$full_file_path[i],
          delim = "\t"
        ) %>% 
        dplyr::mutate(
          cohort = file_df$cohort[i],
          data_type = data_type,
          biomarker = file_df$biomarker[i]
        ) %>% 
        dplyr::select(
          cohort, data_type, biomarker, ID, A1, A1_FREQ, BETA, SE, OBS_CT, P 
        )
      
    } else if(data_type == "lt") {
      
      data_list[[i]] <- 
        readr::read_delim(
          file =
            file_df$full_file_path[i],
          delim = "\t"
        ) %>% 
        dplyr::mutate(
          cohort = file_df$cohort[i],
          data_type = data_type,
          biomarker = file_df$biomarker[i]
        ) %>% 
        dplyr::select(
          cohort, data_type, biomarker, ID, A1, A1_FREQ, BETA = BETAi, SE = SEi, OBS_CT, P = Pi 
        )
      
    } else{
      
      print("Unrecognised data type (i.e. not cross-sectional, cs, or longitudinal, lt")
      
    }
    
  }
  
  # Create merged dataframe
  data_df <- 
    data_list %>% 
    qdapTools::list_df2df(col1 = "list_name") %>% 
    dplyr::select(-list_name)
  
  return(data_df)
  
}

#' Query interface
#' 
#' This function creates the browser to upload the data and the button 
#' the query by SNP
#' 
#' @return browser panel, SNP selection panel
#' 

query_interface <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::helpText("Please, load your data and then query the SNP you want to visualize its effect"),
      
      # To upload the data on the server
      shiny::fileInput("file", NULL, accept = c(".csv", ".tsv", ".txt")),
      # To display an action button to query the user SNP
      shiny::actionButton("add", "Type here to query your SNP")
      
    ),
    shiny::mainPanel(
      shiny::dataTableOutput("GWAS")
    )
  )
  
}
  

#' Get plots
#' 
#' This function will setup the shiny main panel with the cross sectional and 
#' longitudinal plots
#' 
#' @return CS_plot and LT_plot plotOutputs
#' 

get_plots <- function() {
  shiny::mainPanel(
    shiny::tabsetPanel(
      shiny::tabPanel("Cross Sectional",
                      shiny::wellPanel(
                        shiny::verticalLayout(
                          shiny::plotOutput(outputId="CS_plot"),
                          shiny::br(),
                          shiny::downloadButton(outputId="downloadCS_plot",
                                                label="Download plot")
                        )
                      )
      ),
      shiny::tabPanel("Longitudinal",
                      shiny::wellPanel(
                        shiny::verticalLayout(
                          shiny::plotOutput(outputId="LT_plot"),
                          shiny::br(),
                          shiny::downloadButton(outputId="downloadLT_plot",
                                                label="Download plot")
                        )
                      )
      ),
      shiny::tabPanel("Data Table",
                      shiny::verticalLayout(
                        shiny::dataTableOutput("dynamic")
                      )
                    ),
      id = "plots"
    )
  )
}

#' Get forest plot
#'
#' Placeholder function for plotting the GWAS data in a forest plot
#' 
#' @return renders the plot
#' 

get_forest_plot <- function(x, y) {
  # plotting code for forest plot goes here
  plot(x, y)
}

#' This function will set up the side panel ui for the user to enter their query parameters
#' 
#' @return 
#' 

get_sidebar <- function(){
  shiny::sidebarPanel(
    shiny::wellPanel(h3("Enter your query parameters"),
                     shiny::textInput("snp", h4("Variant"),value = "Search by rs ID or chromosome position"),
                     shiny::selectInput("biomarker", h4("Biomarker"),data[,3][!duplicated(data[,3])], selected = 0),
                     shiny::radioButtons("cohort", h4("Cohort"),data[,1][!duplicated(data[,1])],selected = 0),
    ),
    shiny::actionButton("go", "Go"),
  )
}