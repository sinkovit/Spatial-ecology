# This software is Copyright © 2022 The Regents of the University of California.
# All Rights Reserved. Permission to copy, modify, and distribute this software
# and its documentation for educational, research and non-profit purposes,
# without fee, and without a written agreement is hereby granted, provided that
# this entire copyright appear in all copies. Permission to make commercial use
# of this software may be obtained by contacting:
# 
# Office of Innovation and Commercialization
# 9500 Gilman Drive, Mail Code 0910
# University of California
# La Jolla, CA 92093-0910
# (858) 534-5815
# invent@ucsd.edu
#
# This software program and documentation are copyrighted by The Regents of the
# University of California. The software program and documentation are supplied
# “as is”, without any accompanying services from The Regents. The Regents does
# not warrant that the operation of the program will be uninterrupted or
# error-free. The end-user understands that the program was developed for
# research purposes and is advised not to rely exclusively on the program for
# any reason.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
# LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION,
# EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE. THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED
# HEREUNDER IS ON AN “AS IS” BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO
# OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.

library(shiny)
library(mkde)
library(raster)
library(R.utils)
library(shinyjs)
library(shinycssloaders)
library(move)
library(ggplot2)
library(stringr)
library(shinyBS)

source("loadDataframe.R")
source("plotDataframe.R")
source("processDataframe.R")
source("util.R")

sessionInfo()

# Used to disable
callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "table.on('click', 'tbody', function(){",
  "  setTimeout(function(){",
  "    var indexes = table.rows({selected:true}).indexes();",
  "    var indices = Array(indexes.length);",
  "    for(var i = 0; i < indices.length; ++i){",
  "      indices[i] = indexes[i];",
  "    }",
  "    Shiny.setInputValue(id + '_rows_selected', indices);",
  "  }, 0);",
  "});"
)

# Define UI for app
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-size: medium;
        font-weight: bold;
      }
    "))
  ),
  
  # CSS to hide fileInput "Upload Complete"
  # original idea from https://stackoverflow.com/a/49631736
  includeCSS("app.css"),
  
  useShinyjs(), # include shinyjs

  titlePanel ( "Welcome to the Space Use Ecology Gateway!" ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout (

    # Sidebar panel for inputs ----
    sidebarPanel(
      id = "myapp",
      tabsetPanel(
        id = "controls",
        type = "pills",
        header = hr(style = "border-top: 2px solid #000000;"),
        
        tabPanel(
          "1. Load Data",
          value = 1,
          radioButtons("data_source", "Load data from :",
                       choices = c ( "File", "Movebank" ) ),
          conditionalPanel(
            condition = "input.data_source === 'File'",
            fileInput(
              "file_upload", "Upload your GPS data file:", multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain",
                         ".csv")),
            radioButtons("coordinates", label = "Coordinate system:",
                          choices = list("Latitude/Longitude" = 1, "UTM" = 2),
                          selected = 1),
            conditionalPanel (
              condition = "input.coordinates === '2'",
              numericInput ("zone", label = "Zone", value = 30, min = 1, max = 60, step = 1, width = "50%")),
            radioButtons("datum", label = "Datum:",
                         choices = list("NAD 27" = 1, "NAD 83" = 2, "WGS 84" = 3),
                         selected = 3),
          ),
          conditionalPanel(
            condition = "input.data_source === 'Movebank'",
            textInput("movebank_username", "Movebank username"),
            passwordInput("movebank_password", "Password"),
            textInput("movebank_studyid", "Study ID"),
            checkboxInput("save_local", "Save Movebank data locally"),
            bsTooltip(id = "save_local", placement = "right",
                      title = "If local file already exists, will overwrite"),
            conditionalPanel(
              condition = "input.save_local == 1",
              textInput("movebank_local_filename", "Local filename")),
            ),
          hr(style = "border-top: 2px solid #000000;"),
          actionButton("load_data", label = "Load data"),
          actionButton("reset_data", "Reset data"),
        ),
        
        tabPanel(
          "2. Set Parameters",
          value = 2,
          # disable https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
          radioButtons("mode", label = "Mode:",
                       choices = list("2D" = 2, "2.5D" = 1, "3D" = 3),
                       selected = 2),
          tags$strong(id = "sig2obslabel", "sig2obs (meters):"),
          bsTooltip(id = "sig2obslabel", placement = "right",
                    title = "Location error / variance"),
          numericInput("sig2obs", label = "", value = 25.0, width = "50%"),
          tags$strong(id = "tmaxlabel", "Time max (minutes):"),
          bsTooltip(id = "tmaxlabel", placement = "right",
                    title = "Maximum time threshold between consecutive locations"),
          numericInput("tmax", label = "", value = 185.0, width = "50%"),
          #bsPopover(id = "tmax", title = "title", content = "content"),
          numericInput("cellsize", label = "Cell size (meters):", value = 0,
                       min = 1, width = "75%"),
          tags$strong(id = "bufferlabel", "Buffer (meters):"),
          bsTooltip(id = "bufferlabel", placement = "right",
                    title = "Brownian Bridge buffer"),
          numericInput("buffer", label = "", value = 100.0, width = "50%"),
          tags$strong(id = "probabilitylabel", "Cumulative probabilities:"),
          bsTooltip(id = "probabilitylabel", placement = "right",
                    title = "Used to plot probability range; should be comma separated values"),
          textInput ("probability", label = NULL,
                     value = "0.99, 0.95, 0.90, 0.75, 0.5, 0.0"),
          hr(style = "border-top: 2px solid #000000;"),
          actionButton("runx", label = "Run"),
          actionButton("reset_parameters", "Reset parameters"),
        )),
      
	    textOutput ( "debug" ),
	    verbatimTextOutput ( "file_value" ),
    ),

    # Main panel for displaying outputs ----
    mainPanel (
      htmlOutput ( "plot.instructions" ),
      # https://github.com/daattali/shinycssloaders/
      shinycssloaders::withSpinner(plotOutput ( "plot" ), type = 5),
      hr(style = "border-top: 1px solid #000000;"),
      shinycssloaders::withSpinner(DT::dataTableOutput('table'), type = 5)
    ),
  )
)


# Define server logic required
server <- function ( input, output, session ) {

  gps <- reactiveValues()
  
  shinyjs::disable ( "runx" )

  # Update MB local filename based on studyid...
  observeEvent(input$movebank_studyid, {
    updateTextInput(session, "movebank_local_filename",
                    value = paste("movebank-", input$movebank_studyid, ".csv",
                                  sep = ""))
  })
  
  # If the required load data input is missing, disable buttons; otherwise enable...
  observe({
    if ((input$data_source == 'File' && isEmpty(input$file_upload)) ||
        (input$data_source == 'Movebank' && (isEmpty(input$movebank_username) ||
                                             isEmpty(input$movebank_password) ||
                                             isEmpty(input$movebank_studyid)))) {
      shinyjs::disable("load_data")
      shinyjs::disable("reset_data")
    } else {
      shinyjs::enable ( "load_data" )
      shinyjs::enable ( "reset_data" )
    }
  })
  
  # If there is data and cell size is valid and a table row is selected, enable
  # Run button
  observe ({
    # input$table_rows_selected
    if (! isEmpty (gps$data) && input$cellsize >= 1)
      shinyjs::enable ("runx")
    else
      shinyjs::disable ("runx")
  })
  
  table.data <- eventReactive(input$load_data, {
    #shinyjs::disable("runx")
    #shinyjs::hide ( "plot" )
    
    if(input$data_source == 'File') {
      printf("Loading file %s...", input$file_upload$name)
      results = loadDataframeFromFile(input$file_upload$datapath)
      #shinyjs::enable("runx")
      shiny::validate(need(is.null(results[[2]]), results[[2]]))
      printf("done\n")
      data = results[[1]]
    }
    
    else if(input$data_source == 'Movebank') {
      printf("Accessing Movebank...\n")
      #withProgress(message = "Retrieving data from MoveBank...", {
      results <- loadDataframeFromMB(username = input$movebank_username,
                                     password = input$movebank_password,
                                     study = input$movebank_studyid)
      #shinyjs::enable("runx")
      shiny::validate(need(is.null(results[[2]]), results[[2]]))

      data = results[[1]]
    }
    #shinyjs::enable("runx")
    rm(results)

    printf("Preprocessing data...")
    results <- preprocessDataframe(data)
    printf("done\n")
    shiny::validate(need(is.null(results[[2]]), results[[2]]))
    
    data <- results[[1]]
    gps$data <- results[[1]]
    
    # Now save MB data locally
    if(input$data_source == "Movebank" && input$save_local == 1) {
      printf("Saving local file %s...", input$movebank_local_filename)
      result = saveDataframeFromMB(gps$data, input$movebank_local_filename)
      if(is.null(result))
        printf("done\n")
      else
        printf("error: %s\n", result)
    }
    
      # move.stack <- results[[1]]
      # errors <- results[[2]]
      
    #   data.frame$value <- movebankPreprocess(input$sig2obs, input$tmax, move.stack)
    data <- animalAttributes(data, input$cellsize)
    #   shiny::validate(need(!is.null(data), "Error detected in data!"))
    #   
    output$plot.instructions <- renderUI ( {
      tagList (
        h5 ( "To plot:" ),
        tags$ol (
          tags$li("Set parameters (left)"),
          tags$li("Choose an animal (below)"),
          tags$li("Run (left below)")
        ))})
  
    updateTabsetPanel ( session, "controls", selected = "2" )
    
    DT::datatable(
      data[], extensions = 'Buttons',
      #caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
      options = list(
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        stateSave = TRUE),
      selection = list(mode = 'single', target = 'row'))
  })

  output$table <- DT::renderDataTable(table.data())
  
  observeEvent ( input$table_rows_selected, {
    shinyjs::enable ( "runx" )
  })
  
  #mkde.plot <- eventReactive(input$table_rows_selected, {
  mkde.plot <- eventReactive(input$runx, {
    shinyjs::hide ( "plot.instructions" )
    shinyjs::disable("runx")

    if(input$table_rows_selected == "all")
      print("sorry cannot plot all at this time...")
    else {
      # data <- getMKDEData(data.frame$value, input$table_rows_selected,
      #                     input$sig2obs, input$tmax, input$cellsize)
      print(paste("DEBUG: selected row =", input$table_rows_selected))
      data <- gps$data

      # Spatial extent can be calculated in different ways, for example from
      # data set itself, from digital elevation model or manually set. For
      # now, just using min/max values for the GPS readings.
      xmin <- min(data$xdata) - input$buffer
      xmax <- max(data$xdata) + input$buffer
      ymin <- min(data$ydata) - input$buffer
      ymax <- max(data$ydata) + input$buffer

      # Generate a list of rasters
      if (exists ("rasters"))
        print ("DEBUG: rasters exists!")
      else
        print ("DEBUG: no rasters!")
      rasters <- calculateRaster2D(data, input$sig2obs, input$tmax,
                                   input$cellsize, xmin, xmax, ymin, ymax)
      print(paste("DEBUG: rasters length =", length(rasters)))
      tryCatch({
        probs = as.numeric ( unlist (strsplit (input$probability, ",")))
        plotMKDE (rasters[[1]], probs = probs)
      },
      error = function(error_message) {
        print(paste("error_message =", error_message))
        shiny::validate(need(error_message == "",
                             "Unable to plot; please try adjusting the parameter(s) and try again..."))
      },
      finally = {shinyjs::enable("runx")})
    }
  })

  output$plot <- renderPlot({mkde.plot()})

  # output$file_value = renderPrint({
  #   s = input$table_rows_selected
  #   if (length(s)) {
  #     paste('Row:', s)
  #     #cat(s, sep = ', ')
  #   }
  # })

  # Reset sig2obs and t.mat; unfortunately can't "reset" input file
  # (see https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny
  # for a trick)
  # observeEvent ( input$reset, {
  #   shinyjs::reset ( "myapp" )
  #   shinyjs::disable ( "runx" )
  #   shinyjs::disable ( "reset" )
  # } )
  
  observeEvent ( input$reset_data, {
    data_source = input$data_source
    updateRadioButtons ( session, "data_source", selected = data_source )
    shinyjs::reset ( "file_upload" )
    shinyjs::reset ( "movebank_username" )
    shinyjs::reset ( "movebank_password" )
    shinyjs::reset ( "movebank_studyid" )
    shinyjs::reset ( "save_local" )
    shinyjs::disable ( "load_data" )
    shinyjs::disable ( "reset_data" )
  } )
}

shinyApp ( ui = ui, server = server )

