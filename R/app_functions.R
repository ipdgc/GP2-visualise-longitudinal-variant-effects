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

#' Get forest plot and observations count plot
#'
#' @description This function will output two plots: (i) a forest plot of betas
#'   for a biomarker across cohorts and (ii) a bar plot of the number of
#'   participants/observations (depending on the data type) across each cohort.
#' 
#' @param filtered_data a filtered `data.frame` or
#'   [tibble][tibble::tbl_df-class] object, with the following columns:
#'  \itemize{
#'  \item `cohort`: the study cohort.
#'  \item `data_type`: the type of data (either cross-sectional or longitudinal.
#'  Should be one of `c("cs", "lt")`.
#'  \item `biomarker`: the biomarker studied.
#'  \item `ID`: SNP in the format chromosome:basepair:reference:alternate.
#'  \item `BETA`: the beta coefficient
#'  \item `SE`: the standard error of the beta
#'  \item `OBS_CT`: the number of participants (if cross-sectional) or total
#'  observations (if longitudinal).
#'  }
#'  The `data.frame`/[tibble][tibble::tbl_df-class] should be filtered to
#'  include only one `ID` and one type of data (one of `c("cs", "lt")`).
#'  
#' @return `ggplot` displaying a forest plot and 
#'   terms. \itemize{ 
#'   \item x-axis on the forest plot displays the beta while on the bar plot
#'   displays the number of particpants/observations.
#'   \item y-axis on both plots displays the cohort 
#'   \item dot on the forest plot indicates the beta, while lines indicate the beta +/- the standard error
#'   }
#'   

get_forest_plot <- function(filtered_data) {
  
  # Add if/else
  if(unique(filtered_data$data_type) == "cs"){
    
    xlab_forest <- "Cross-sectional beta (+/- standard error)"
    xlab_obs <- "Number of participants"
    
    
  } else if(unique(filtered_data$data_type) == "lt"){
    
    xlab_forest <- "Longitudinal beta (+/- standard error)"
    xlab_obs <- "Total number of observations"
    
  } else{
    
    print("Unrecognised data type (i.e. not cross-sectional, cs, or longitudinal, lt")
    
  }
  
  # Change cohort and biomarker names to human-readable format
  plot_data <- 
    filtered_data %>% 
    dplyr::mutate(
      cohort = stringr::str_replace_all(cohort, "_", " "),
      biomarker = stringr::str_replace_all(biomarker, "_", " ")
    )
  
  # Determine max. observations for later nudging of text
  max_obs <- max(plot_data$OBS_CT)
  
  # Plot forest plot
  forest_plot <- 
    plot_data %>% 
    dplyr::mutate(
      cohort = stringr::str_replace_all(cohort, "_", " "),
      biomarker = stringr::str_replace_all(biomarker, "_", " ")
    ) %>% 
    ggforestplot::forestplot(
      name = cohort,
      estimate = BETA,
      se = SE,
      xlab = xlab_forest, 
      ylab = "Cohort"
    ) +
    ggforestplot::theme_forest(base_size = 12) + 
    ggforce::facet_col(
      facets = ~biomarker,
      scales = "free_y",
      space = "free"
    )
  
  # Plot observations
  obs_count_plot <-
    plot_data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = OBS_CT,
        y = cohort
      )
    ) + 
    ggplot2::geom_col(
      colour = "black"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = OBS_CT, 
        # Add a tiny bit on to the OBS_CT value to nudge the text
        x = OBS_CT + max_obs/10
      )
    ) +
    ggplot2::labs(
      x = xlab_obs
    ) +
    ggplot2::theme_classic(base_size = 12) +
    # ggforestplot::theme_forest(base_size = 12) +
    ggplot2::theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
  
  # Combine plots  
  combined_plot <-
    cowplot::plot_grid(
      forest_plot,
      obs_count_plot,
      align = c("h"),
      axis = "bt", 
      rel_widths = c(2,1)
    )
  
  return(combined_plot)
  
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