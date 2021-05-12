# This is Shiny web application, with the aim of visualising cross-sectional and longitudinal variant effects.
# Run locally using the "Run App" button.
# For a guide to R Shiny Apps, see: https://mastering-shiny.org/index.html

#----Load libraries------------------------------------------------
library(here) # For easy construction of file paths
library(shiny) # For shiny web app functions
library(shinythemes) # For shiny themes
library(tidyverse) # For data wrangling
library(stringr) # For string manipulation
library(qdapTools) # For list/dataframe manipulation
library(waiter) # For wait screens

waiting_screen <-
  shiny::tagList(
    h2("Loading page"), br(),
    spin_balance()
  )

#----Load data and functions---------------------------------------

# Functions
source(here::here("R", "app_functions.R"))

# Data
data <-
  get_toy_data()

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
        tags$h2("Welcome to the GP2/IPDGC 2021 Hackathon's Cross Sectional and Longitudinal GWAS Summary Statistics Visualization Tool!"),
        tags$h4("This application aims to use the results from biomarker Genome Wide Association Studies (GWAS) and enable researchers to query a particular biomarker and obtain a visualization of the of the biomarker effect on all or a set of cohorts, as well as, the associated meta-analysis. The app is also capable of displaying longitudinal information alongside the cross-sectional results."),
        tags$h6('Project Members:
                Michael Ta
                - Regina Reynolds
                - Teresa Periñan
                - Alejandro Carrasco
                - Clodagh Towns
                - Nikita Pillay'),
                
        # Include logos
        get_logos()

      ),

      # Plot panel
      # Main panel with plots
      shiny::tabPanel(
        title = "Plots",
        
        get_plots(),
        
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
    
    # ---- Drawing Plots & Other Outputs----------------------------
    output$CS_plot <- renderPlot({
      # TODO: current placeholder plotting function, pass in GWAS summary stats 
      get_forest_plot(1,10)
    },execOnResize = TRUE)
    
    output$LT_plot <- renderPlot({
      # TODO: current placeholder plotting function, pass in GWAS summary stats
      get_forest_plot(1,5)
    },execOnResize = TRUE)
    
    # Test
    output$dynamic <- renderDataTable(data, options = list(pageLength = 5))
    
    # ---- Exporting Plots------------------------------------------
    ### Download LT plot 
    output$downloadLT_plot <- downloadHandler(
      filename = function(){
        # TODO: change input$snp and input$biomarker
        paste(input$snp, input$biomarker, "ltplot.pdf", sep = "_")
      },
      content = function(file){
        pdf(file)
        # TODO: current placeholder plotting function, pass in GWAS summary stats
        #get_forest_plot(1,10)
        dev.off()
      }
    )
    
    ### Download CS plot 
    output$downloadCS_plot <- downloadHandler(
      filename = function(){
        # TODO: change input$snp and input$biomarker
        paste(input$snp, input$biomarker, "csplot.pdf", sep = "_")
      },
      content = function(file){
        pdf(file)
        # TODO: current placeholder plotting function, pass in GWAS summary stats
        #get_forest_plot(1,10)
        dev.off()
      }
    )
    
  }

# Launch app
shiny::shinyApp(ui, server)
