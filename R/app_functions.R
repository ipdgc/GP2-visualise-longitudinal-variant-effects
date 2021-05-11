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
    p(
      img(src="ipdgc_logo.png", width="20%"), 
      img(src="gp2_logo.png", width="20%"),
      align="center"
    )
  )
  
}
  
  