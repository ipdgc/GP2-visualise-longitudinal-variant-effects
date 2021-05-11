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
  
  