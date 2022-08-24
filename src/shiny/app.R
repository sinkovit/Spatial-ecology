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

source("gps.R")
source("movebank.R")
source("util.R")
source("loadDataFrame.R")

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
  
  useShinyjs(), # include shinyjs

  titlePanel ( "Welcome to the Space Use Ecology Gateway!" ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout (

    # Sidebar panel for inputs ----
    sidebarPanel (
      id = "myapp",
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "1. Load Data", hr(style = "border-top: 1px solid #000000;"),
          radioButtons("data_source", "Load data from :",
                       choices = c("File", "Movebank")),
          conditionalPanel(
            condition = "input.data_source === 'File'",
            fileInput(
              "file.upload", "Upload your GPS data file:", multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain",
                         ".csv"))),
          conditionalPanel(
            condition = "input.data_source === 'Movebank'",
            textInput("movebank.username", "Movebank Username", value = "mona",
                      width = NULL, placeholder = NULL),
            passwordInput("movebank.password", "Password", value = "g0MB2022",
                          width = NULL, placeholder = NULL),
            textInput("movebank.studyid", "Study ID", value = "408181528",
                      width = NULL, placeholder = NULL))),
        
        tabPanel(
          "2. Set Parameters", hr(style = "border-top: 1px solid #000000;"),
          # disable https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
          radioButtons("radio", label = "Mode:",
                       choices = list("2D" = 2, "2.5D" = 1, "3D" = 3),
                       selected = 2),
          tags$strong(id = "sig2obslabel", "sig2obs (meters):"),
          bsTooltip(id = "sig2obslabel", placement = "right",
                    title = "Location error / variance"),
          numericInput("sig2obs", label = "", value = 25.0),
          tags$strong(id = "tmaxlabel", "t.max (minutes):"),
          bsTooltip(id = "tmaxlabel", placement = "right",
                    title = "Maximum time threshold between consecutive locations"),
          numericInput("tmax", label = "", value = 185.0),
          #bsPopover(id = "tmax", title = "title", content = "content"),
          numericInput("cellsize", label = "Cell size (meters)", value = 3000)
        )),
      
      hr(style = "border-top: 2px solid #000000;"),
      actionButton("runx", label = "Run"),
      actionButton("reset", "Reset"),

	    textOutput ( "debug" ),
	  
	    verbatimTextOutput ( "file_value" ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # https://github.com/daattali/shinycssloaders/
      shinycssloaders::withSpinner(plotOutput ( "plot" ), type = 5),
      hr(style = "border-top: 1px solid #000000;"),
      textOutput("table.info"),
      #hr(style = "border-top: 1px solid #000000;"),
      #htmlOutput("table.info"),
      shinycssloaders::withSpinner(DT::dataTableOutput('table'), type = 5)
    ),
  )
)


# Define server logic required
server <- function ( input, output, session ) {

  shinyjs::disable("radio")
  
  #output$status <- renderPrint({"Please load your data from either MoveBank or browse to a local file..."})
  
  # If a required input is missing, disable Run button; otherwise enable...
  observe({
    if ((input$data_source == 'File' && isEmpty(input$file.upload)) ||
        (input$data_source == 'Movebank' && (isEmpty(input$movebank.username) ||
                                             isEmpty(input$movebank.password) ||
                                             isEmpty(input$movebank.studyid)))) {
      shinyjs::disable ( "runx" )
    } else {
      shinyjs::enable ( "runx" )
      shinyjs::enable ( "reset" )
    }
  })
  
  data.frame <- reactiveValues()
  
  table.data <- eventReactive(input$runx, {
    shinyjs::disable("runx")
    
    if(input$data_source == 'File') {
      printf("Loading file %s...", input$file.upload$name)
      results = loadDataFrameFromFile(input$file.upload$datapath)
      
      # error handling
      if(!is.null(results[[2]])) {
        shinyjs::enable("runx")
        shiny::validate(need(is.null(results[[2]]), results[[2]]))
      }
      
      printf("done\n")
      data = results[[1]]
      #print(paste("data =", data))
    }
    
    else if(input$data_source == 'Movebank') {
      printf("Accessing Movebank...\n")
      #withProgress(message = "Retrieving data from MoveBank...", {
      results <- loadDataFrameFromMB(username = input$movebank.username,
                                     password = input$movebank.password,
                                     study = input$movebank.studyid)
      #})
      #print("Done")
      #shiny::validate(need(results[[2]] == "", results[[2]]))

      # error handling
      if (!is.null(results[[2]])) {
        shinyjs::enable("runx")
        shiny::validate(need(results[[2]] == "", results[[2]]))
      }
      
      move.stack <- results[[1]]
      errors <- results[[2]]
      
      data.frame$value <- movebankPreprocess(input$sig2obs, input$tmax, move.stack)
      data <- animalAttributes(data.frame$value)
      shiny::validate(need(!is.null(data), "Error detected in data!"))
      
      output$table.info <-
        renderText({"The following table gives the extent of animal movement for the individuals and for the data set as a whole, along with the grid dimensions resulting from various cell sizes. Keep in mind that larger grids result in longer calculations, so you may want to choose a cell size that result in a smaller grid for preliminary calculations.\n E-W(m) and N-S(m) are the east-west and north-south ranges, in meters px(m) is the pixel size in meters and grid is the resulting grid dimensions.\n\n"})
      
      DT::datatable(
        data[], extensions = 'Buttons',
        caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
        options = list(
          autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
          pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
          stateSave = TRUE),
        selection = list(mode = 'single', selected = c(1), target = 'row'))
    }})
  
  output$table <- DT::renderDataTable(table.data())
  
  mkde.plot <- eventReactive(input$table_rows_selected, {
    shinyjs::disable("runx")
    
    if(input$table_rows_selected == "all")
      print("sorry cannot plot all at this time...")
    else {
      data <- getMKDEData(data.frame$value, input$table_rows_selected,
                          input$sig2obs, input$tmax, input$cellsize)
      tryCatch({
        plotMKDE(data)
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
  observeEvent ( input$reset, {
    shinyjs::reset ( "myapp" )
    shinyjs::disable ( "runx" )
    shinyjs::disable ( "reset" )
  } )
}

shinyApp ( ui = ui, server = server )

