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
library(shinydashboard) # https://rstudio.github.io/shinydashboard/index.html
library(shinyFiles) # server-side file browser; see https://rdrr.io/cran/shinyFiles/

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
#ui <- fluidPage(
ui <- dashboardPage(
  
  # skin = "black",
  # title = "Space Use Ecology",
  
  # code from https://stackoverflow.com/a/41145602
  dashboardHeader(#title = "Space Use Ecology Gateway",
    #title = span("Space Use Ecology Gateway", style = "color: black;"),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css",
    #                     href = "custom.css")),
    # tags$li(a(href = 'http://shinyapps.company.com',
    #           icon("power-off"),
    #           title = "Back to Apps Home"),
    #         class = "dropdown"),
    
    title = a(href = "https://uccommunityhub.hubzero.org/groups/spaceuseecology",
              img(src = "logo.png"))

    # tags$li(
    #   a(href = 'https://uccommunityhub.hubzero.org/groups/spaceuseecology',
    #     img(src = 'logo.png', title = "Space Use Ecology Gateway",
    #         #height = "52px"), style = "padding: 0px"),
    #         )),
    #   class = "dropdown")
    ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "custom.css")),
    
  
  # tags$head(
  #   tags$style(HTML("
  #     .shiny-output-error-validation {
  #       color: red;
  #       font-size: medium;
  #       font-weight: bold;
  #     }
  #   "))
  # ),
  
  # CSS to hide fileInput "Upload Complete"
  # original idea from https://stackoverflow.com/a/49631736
  includeCSS("app.css"),
  
  useShinyjs(), # include shinyjs

  # titlePanel ( h3 ("Welcome to the Space Use Ecology Gateway!",
  #                  align = "center" )),
  
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
          radioButtons ("data_source", "Load data from :",
                       choices = c ("Gateway", "Movebank", "Your computer")),

          conditionalPanel (
            condition = "input.data_source === 'Gateway'",
            fluidRow (id = "gateway_browse_row",
                      column (id = "gateway_browse_button", width = 4, offset = 0,
                              #style='padding:0px', 
                              shinyFilesButton ('gateway_file', label='Browse',
                                                title='Select your GPS data file',
                                                multiple=FALSE, viewtype = "detail")),
                      column (id = "gateway_browse_file", width = 8, offset = 0,
                              htmlOutput ("gateway_file_display"))),
            tags$p()),

          conditionalPanel(
            condition = "input.data_source === 'Movebank'",
            textInput("movebank_username", "Movebank username"),
            passwordInput("movebank_password", "Password"),
            textInput("movebank_studyid", "Study ID"),
            checkboxInput ("movebank_save_local",
                           "Save Movebank data to your computer"),
            bsTooltip(id = "movebank_save_local", placement = "right",
                      title = "If local file already exists, will overwrite"),
            conditionalPanel(
              condition = "input.movebank_save_local == 1",
              textInput("movebank_local_filename", "Local filename")),
          ),
          
          conditionalPanel (
            condition = "input.data_source === 'Your computer'",
            fileInput ("local_file", label = NULL,
                       multiple = FALSE, buttonLabel = "Browse",
                       accept = c ("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))),

          conditionalPanel (
            condition = "input.data_source === 'Gateway' || input.data_source === 'Your computer'",
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
          
          hr(style = "border-top: 2px solid #000000;"),
          actionButton("load_data", label = "Load data"),
          actionButton("reset_data", "Reset data"),
        ),
        
        tabPanel(
          "2. Set Parameters",
          value = 2,
          tags$strong (id = "modelabel", "Mode:"),
          bsTooltip (id = "modelabel", placement = "right",
                     title = "Currently only 2D is supported but we are planning on adding 2.5 and 3D"),
          # doesn't work: https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
          radioButtons ("mode", label = NULL,
                        choices = list ("2D" = '2D', "2.5D" = '25D', "3D" = '3D')),
          tags$strong(id = "sig2obslabel", "sig2obs (meters):"),
          bsTooltip(id = "sig2obslabel", placement = "right",
                    title = "Location error / variance"),
          numericInput ("sig2obs", label = NULL, value = 25.0, width = "50%"),
          tags$strong(id = "tmaxlabel", "Time max (minutes):"),
          bsTooltip(id = "tmaxlabel", placement = "right",
                    title = "Maximum time threshold between consecutive locations"),
          numericInput ("tmax", label = NULL, value = 185.0, width = "50%"),
          #bsPopover(id = "tmax", title = "title", content = "content"),
          numericInput("cellsize", label = "Cell size (meters):", value = 0,
                       min = 1, width = "75%"),
          tags$strong(id = "bufferlabel", "Buffer (meters):"),
          bsTooltip(id = "bufferlabel", placement = "right",
                    title = "Brownian Bridge buffer"),
          numericInput ("buffer", label = NULL, value = 100.0, width = "50%"),
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
      #plotOutput ("plot"),
      #hr(style = "border-top: 2px solid #000000;"),
      
      tabsetPanel(
        id = "tables",
        type = "pills",
        #header = hr(style = "border-top: 1px solid #000000;"),

        tabPanel (
          "All",
          value = 1,
          shinycssloaders::withSpinner (DT::dataTableOutput ('table_all'), type = 5)),
        tabPanel (
          "Summary",
          value = 2,
          shinycssloaders::withSpinner (DT::dataTableOutput ('table_summary'), type = 5)))
    )
  )
  )
)


# Define server logic required
server <- function ( input, output, session ) {

  gps <- reactiveValues()
  clear_plot <- reactiveVal (FALSE)
  
  shinyjs::hide ("tables")
  shinyjs::disable (selector = "[type=radio][value=25D]")
  shinyjs::disable (selector = "[type=radio][value=3D]")
  
  # Check parameters, if invalid will turn border to red, otherwise no color
  observe ({
    if (!is.numeric (input$sig2obs) || input$sig2obs <= 0) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('sig2obs').style.border ='", color,
                   "'"))

    if (!is.numeric (input$tmax) || input$tmax <= 0) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('tmax').style.border ='", color,
                   "'"))

    if (!is.numeric (input$cellsize) || input$cellsize <= 0) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('cellsize').style.border ='", color,
                   "'"))

    if (!is.numeric (input$buffer) || input$buffer < 0) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('buffer').style.border ='", color,
                   "'"))
    
    # if (isEmpty (input$probability) ||
    #     regexpr ("([0-9].[0-9][0-9],?\s?)+", input$probability, ignore.case = TRUE) == -1) {
    #   color <- "solid #FF0000"
    # } else {
    #   color <- ""
    # }
    # runjs (paste0 ("document.getElementById('probability').style.border ='", color,
    #                "'"))
  })
  
  # setup & display the gateway browser & selected file
  # volumes <- c (Home = fs::path_home(), "R Installation" = R.home(),
  #               getVolumes()())
  gateway_volumes <- c (Home = fs::path_home())
  shinyFileChoose (input, "gateway_file", roots = gateway_volumes,
                   session = session)
  output$gateway_file_display <-
    renderUI ({
      if (is.integer (input$gateway_file)) {
        HTML ("No file selected")
      } else {
        tmp <- parseFilePaths (gateway_volumes, input$gateway_file)
        HTML (paste ("<font color=\"#545454\">", tmp$name, "</font>"))
      }})

  # Update MB local filename based on studyid...
  observeEvent(input$movebank_studyid, {
    updateTextInput(session, "movebank_local_filename",
                    value = paste("movebank-", input$movebank_studyid, ".csv",
                                  sep = ""))
  })
  
  # If the required load data input is missing, disable buttons; otherwise enable...
  observe({
    if ((input$data_source == 'Gateway') &&
        isEmpty (parseFilePaths (gateway_volumes, input$gateway_file)$name) ||
        (input$data_source == 'Your computer' && isEmpty(input$local_file)) ||
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
    # input$table_summary_rows_selected
    if (! isEmpty (gps$original)  &&
       is.numeric(input$sig2obs)  && input$sig2obs >= 0 &&
       is.numeric(input$tmax)     && input$tmax >= 0 &&
       is.numeric(input$cellsize) && input$cellsize >= 1 &&
       is.numeric(input$buffer)   && input$buffer >= 0)
      shinyjs::enable ("runx")
    else
      shinyjs::disable ("runx")
  })
  
  observeEvent (input$load_data, {
    
    # if there are rasters, then we need to clear the data and plot
    if (! is.null (gps$rasters)) {
      clear_plot (TRUE)
      gps$data <- NULL
      gps$original <- NULL
      gps$rasters <- NULL
      gps$summary <- NULL
      shinyjs::show ("plot.instructions")
    }

    if(input$data_source == 'Gateway') {
      file <- parseFilePaths (gateway_volumes, input$gateway_file)
      printf ("Loading gateway file %s...", file$name)
      results = loadDataframeFromFile (file$datapath)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))
      printf("done\n")
      data = results[[1]]
    }

    else if(input$data_source == 'Movebank') {
      printf("Accessing Movebank...\n")
      results <- loadDataframeFromMB(username = input$movebank_username,
                                     password = input$movebank_password,
                                     study = input$movebank_studyid)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))

      data = results[[1]]
    }
    
    else if(input$data_source == 'Your computer') {
      printf("Loading local file %s...", input$local_file$name)
      results = loadDataframeFromFile(input$local_file$datapath)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))
      printf("done\n")
      data = results[[1]]
    }
    
    rm(results)

    printf("Preprocessing data...")
    results <- preprocessDataframe(data)
    printf("done\n")
    shiny::validate(need(is.null(results[[2]]), results[[2]]))
    
    data <- results[[1]]
    gps$data <- results[[1]]
    gps$original <- results[[1]]
    
    # Now save MB data locally
    if(input$data_source == "Movebank" && input$movebank_save_local == 1) {
      printf("Saving local file %s...", input$movebank_local_filename)
      result = saveDataframeFromMB(gps$original, input$movebank_local_filename)
      if(is.null(result))
        printf("done\n")
      else
        printf("error: %s\n", result)
    }
    
    data <- animalAttributes(data, input$cellsize)
    gps$summary <- data
    updateTabsetPanel ( session, "tables", selected = "2" )
    
    output$plot.instructions <- renderUI ( {
      tagList (
        h5 ( "To plot:" ),
        tags$ol (
          tags$li("Set parameters (left)"),
          tags$li("Choose an animal from Summary table (below)"),
          tags$li("Run (left below)")
        ))})
  
    updateTabsetPanel ( session, "controls", selected = "2" )
    shinyjs::show ("tables")
  })
  
  table_all.data <- eventReactive (gps$original, {
    DT::datatable (
      gps$original[], extensions = 'Buttons',
      #caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
      options = list (
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        stateSave = TRUE),
      selection = list(mode = 'single', target = 'row'))
    })
  
  table_summary.data <- eventReactive (gps$summary, {
    shinyjs::show ("tables")

    DT::datatable (
      gps$summary[], extension = "Buttons",
      #caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
      options = list(
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        stateSave = TRUE),
      selection = list(mode = 'single', selected = 1, target = 'row'))
  })
  
  output$table_all <- DT::renderDataTable(table_all.data())
  output$table_summary <- DT::renderDataTable(table_summary.data())
  
  mkde.plot <- eventReactive (input$runx, {
    shinyjs::hide ( "plot.instructions" )
    shinyjs::disable("runx")

    summary <- gps$summary
    id <- summary$id[input$table_summary_rows_selected]

    if (id == "all") {
      shinyjs::enable("runx")
      shiny::validate (need (id != "all",
                             "Sorry cannot plot multiple animals currently..."))
    } else {
      data <- gps$data

      # Spatial extent can be calculated in different ways, for example from
      # data set itself, from digital elevation model or manually set. For
      # now, just using min/max values for the GPS readings.
      xmin <- min(data$xdata) - input$buffer
      xmax <- max(data$xdata) + input$buffer
      ymin <- min(data$ydata) - input$buffer
      ymax <- max(data$ydata) + input$buffer

      rasters <- gps$rasters
      raster <- NULL
      
      if (! is.null (rasters) && ! is.null (rasters[[id]])) {
        raster <- rasters[[id]]
      } else {
        raster <- calculateRaster2D (data, id, input$sig2obs, input$tmax,
                                     input$cellsize, xmin, xmax, ymin, ymax)
        rasters[[id]] <- raster
        gps$rasters <- rasters
      }
      
      tryCatch({
        probs = as.numeric ( unlist (strsplit (input$probability, ",")))
        plotMKDE (raster, probs = probs, asp = rasters[[1]]$ny/rasters[[1]]$nx,
                  xlab='', ylab='')
      },
      error = function(error_message) {
        print(paste("error_message =", error_message))
        shiny::validate(need(error_message == "",
                             "Unable to plot; please try adjusting the parameter(s) and try again..."))
      },
      finally = {shinyjs::enable("runx")})
    }
  })

  output$plot <- renderPlot ({
    if (clear_plot()) {
      clear_plot (FALSE)
      return()
    }
    else
      mkde.plot()
  })

  observeEvent ( input$reset_data, {
    data_source = input$data_source
    updateRadioButtons ( session, "data_source", selected = data_source )
    shinyjs::reset ( "local_file" )
    #shinyjs::reset ("gateway_file_display")
    shinyjs::reset ( "movebank_username" )
    shinyjs::reset ( "movebank_password" )
    shinyjs::reset ( "movebank_studyid" )
    shinyjs::reset ( "movebank_save_local" )
    shinyjs::disable ( "load_data" )
    shinyjs::disable ( "reset_data" )
  } )
}

shinyApp ( ui = ui, server = server )

