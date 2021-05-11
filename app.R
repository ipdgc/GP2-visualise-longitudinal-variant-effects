# This is Shiny web application, with the aim of visualising cross-sectional and longitudinal variant effects.
# Run locally using the "Run App" button.
# For a guide to R Shiny Apps, see: https://mastering-shiny.org/index.html

#----Load libraries------------------------------------------------
library(here) # For easy construction of file paths
library(shiny) # For shiny web app functions
library(shinythemes) # For shiny themes
library(waiter) # For wait screens

waiting_screen <- 
  shiny::tagList(
    h2("Loading page"), br(),
    spin_balance()
  ) 

#----Load data and functions---------------------------------------

# Data


# Functions
source(here::here("R", "app_functions.R"))

#----User interface------------------------------------------------

ui <- 
  shiny::fluidPage(
    
    # Add waiting screen
    waiter::use_waiter(spinners = 5),
    waiter::waiter_show_on_load(html = waiting_screen, color = "black"),
    waiter::use_waitress(percent_color = "#006600", color = "#E3EBEE"),
    
    # Set theme
    theme <- shinythemes::shinytheme("spacelab"),
    
    # Navigation bar
    shiny::navbarPage(
      title = "Visualise cross-sectional and longitudinal variant effects", 
      windowTitle = "",
      
      # About panel
      # Brief description of app
      shiny::tabPanel(
        title = "About",
        
        # Include logos
        get_logos()
        
      ),
      
      # Plot panel
      # Main panel with plots
      shiny::tabPanel(
        title = "Plots",
        
        # Include logos
        get_logos()
        
      ),
      
      # Cohort panel
      # Provide a description of the cohorts
      shiny::tabPanel(
        title = "Cohorts",
        
        # Include logos
        get_logos()
        
      ),
      
      # Contact panel
      shiny::tabPanel(
        title = "Contact",
        
        # Include logos
        get_logos()
        
      )
      
    ) # end navigation
    
    
  ) # end ui

#----Server function-----------------------------------------------

server <- 
  function(
    input, 
    output, 
    session
  ) {
    
    # Hide waiting screen upon loading
    waiter::waiter_hide()
    
  }

# Launch app
shiny::shinyApp(ui, server)