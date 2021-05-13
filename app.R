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
        
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            shiny::wellPanel(
              shiny::h4(shiny::strong("Welcome to the GP2/IPDGC 2021 Hackathon's Cross Sectional and Longitudinal GWAS Summary Statistics Visualization Tool!")), 
              shiny::br(),
              shiny::h4("This application aims to use the results from biomarker Genome Wide Association Studies (GWAS) and enable researchers to query a particular biomarker and obtain a visualization of the of the biomarker effect on all or a set of cohorts, as well as, the associated meta-analysis. The app is also capable of displaying longitudinal information alongside the cross-sectional results."), 
              shiny::br(),
              shiny::h6("Project Members:
                Michael Ta
                - Regina Reynolds
                - Teresa Periñan
                - Alejandro Carrasco
                - Clodagh Towns
                - Nikita Pillay", 
                        align="center")
            ), # well panel
            # Include logos
            get_logos(),
            shiny::hr()
          )
        )

      ),
      
      # Filtering the input data
      shiny::tabPanel(
        title = "Upload data",
        
        get_upload_interface(),
        
        # Include logos
        get_logos()
      
        ),

      # Plot panel
      # Main panel with plots
      shiny::tabPanel(
        title = "Plots",
        
        # Sidebar and plot panels
        shiny::sidebarLayout(
          get_sidebar(),
          get_plots()
        ),
        
        # Include logos
        get_logos()

      ),

      # Cohort panel
      # Provide a description of the cohorts
      shiny::tabPanel(
        title = "Cohorts",
        
        shiny::wellPanel(
          
          shiny::h4("Click on the cohort name for further information"),
          shiny::br(),
          shiny::p(
            shiny::a(href="https://amp-pd.org/unified-cohorts/ppmi#inclusion-and-exclusion-criteria", 
                     shiny::h4(
                       shiny::strong("PPMI"),"(Parkinson's Progression Markers Initiative)")
            )
          ),
          shiny::p(
            shiny::a(href="https://amp-pd.org/unified-cohorts/ppmi#inclusion-and-exclusion-criteria", 
                     shiny::h4(
                       shiny::strong("BioFIND"),"(Fox Investigation for New Discovery of Biomarkers in Parkinson's Disease)"))
          )
          
        ),

        # Include logos
        get_logos()

      ),

      # Contact panel
      shiny::tabPanel(
        title = "Contact",
        
        shiny::wellPanel(
          
          # Include logos
          get_logos()
          
        )

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
    
    # ---- Getting the uploaded data-------------------------------
    
    raw_data <- reactive({
      req(input$file)
      
      ext <- tools::file_ext(input$file$name)
      switch(ext,
             csv = vroom::vroom(input$file$datapath, delim = ","),
             tsv = vroom::vroom(input$file$datapath, delim = "\t"),
             txt = vroom::vroom(input$file$datapath, delim = "\t"),
             validate("Invalid file; Please upload a .csv or .tsv, or .txt file")
      )
    })
    # TODO: Add functionality to load inhouse data
    
    # ---- Showing the data as data Table---------------------------
    output$GWAS <- 
      shiny::renderDataTable({
        
        raw_data() %>% arrange(.data$P)
        
      })

    
    # ---- Define Event Observers -----------------------------------------
    # These sections of code are called whenever the user interacts with the UI
    # event listeners will trigger reactive functions
    
    # ---- Adding the textInput widget when the action button is used ---------------
    shiny::observeEvent(input$add, {
      shiny::insertUI(
        selector = "#add",
        where = "afterEnd",
        ui = textInput("SNP",
                       "Insert the SNP you want to query")
      )
    })
    
    shiny::observeEvent(input$snp, {
      hide_tabs()
    })
    
    shiny::observeEvent(input$biomarker, {
      hide_tabs()
    })
    
    # ---- Define reactive components -----------------------------------------
    # These sections of code are called whenever the user interacts with the UI
    # and any changes that we have to make due to user interaction
    
    # ---- Data query for toy dataset -----------------------------------------
    filtered_data <- reactive({
      req(input$biomarker)
      #req(input$cohort)
      # create an empty dataframe with the columns we expect and 
      # we can filter the input$snp query ourselves to either show the complete table
      # if no query returned or show only the results
      df <- data.frame(columns=colnames(data))
      if ((input$snp == "Search by rs ID or chromosome position") || (input$snp == "")) {
        df <- data
      } else {
        df <- data %>%
          filter(.data$ID == .env$input$snp,
                 .data$biomarker %in% .env$input$biomarker)
                 #.data$cohort %in% .env$input$cohort)
      }
      df
    })
    # TODO: Add the filtering function on the app_functions.R instead of here.
    # TODO: Make the filtering function more versatile - Allow to filter by biomarker if the user wants to plot by biomarker?
    # TODO: Add cohort filtering - for selection check boxes for some reason the table breaks when cohort is included
    
    hide_tabs <- reactive({
      req(input$biomarker)
      df <- filtered_data() # grab the filtered data
      if (nrow(df) < 1) {
        shiny::hideTab(inputId="plots", target="Longitudinal")
        shiny::hideTab(inputId="plots", target="Cross Sectional")
      } else {
        if (has_data_type(df, 'lt')) {
          shiny::showTab(inputId="plots", target="Longitudinal")
        }
        if (has_data_type(df, 'cs')) {
          shiny::showTab(inputId="plots", target="Cross Sectional")
        }
  
        if ((input$snp == "Search by rs ID or chromosome position") || (input$snp == "")) {
          shiny::hideTab(inputId="plots", target="Longitudinal")
          shiny::hideTab(inputId="plots", target="Cross Sectional")
        }
      }
    })
    
    # ---- Drawing Plots & Other Outputs----------------------------
    output$CS_plot <- 
      shiny::renderPlot({
        get_forest_plot(filtered_data() %>% filter(data_type == 'cs'))
      },
      execOnResize = TRUE
      )
    
    output$LT_plot <- 
      shiny::renderPlot({
        get_forest_plot(filtered_data() %>% filter(data_type == 'lt'))
      },
      execOnResize = TRUE
      )
    
    output$dynamic <- renderDataTable(filtered_data(), options = list(pageLength = 5))
    
    # ---- Exporting Plots------------------------------------------
    ### Download LT plot 
    output$downloadLT_plot <- 
      shiny::downloadHandler(
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
    output$downloadCS_plot <- 
      shiny::downloadHandler(
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
    
  } # end server

# Launch app
shiny::shinyApp(ui, server)
