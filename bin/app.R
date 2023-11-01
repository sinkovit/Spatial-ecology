# This software is Copyright 2022 The Regents of the University of California.
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

# This code uses the Tidyverse Style Guide @ https://style.tidyverse.org

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
# library(shinyWidgets) # https://dreamrs.github.io/shinyWidgets/index.html &
#                       # https://shinyapps.dreamrs.fr/shinyWidgets/

source("compute.R")
source("loadDataframe.R")
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
  
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
    #includeCSS("app.css"),
    
    useShinyjs(), # include shinyjs
  
    # titlePanel ( h3 ("Welcome to the Space Use Ecology Gateway!",
    #                  align = "center" )),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(id = "inputs",
        tabsetPanel(id = "controls", type = "tabs",
          tabPanel(title = "Data", value = "Data", tags$br(),
            radioButtons("data_source", "Load data from:",
              choices = c ("Gateway", "Movebank", "Your computer")
            ),
            conditionalPanel(condition = "input.data_source === 'Gateway'",
              shinyFilesButton('gateway_browse', label='Browse',
                               title = 'Select your GPS data file',
                               multiple = FALSE, viewtype = "detail"
              ),
              htmlOutput("gateway_file", TRUE),
              tags$p()
            ),
            conditionalPanel(condition = "input.data_source === 'Movebank'",
              textInput("movebank_username", "Movebank username"),
              passwordInput("movebank_password", "Password"),
              textInput("movebank_studyid", "Study ID"),
              checkboxInput("movebank_save_local",
                "Save Movebank data to your computer"
              ),
              bsTooltip(id = "movebank_save_local", placement = "right",
                title = "If local file already exists, will overwrite"
              ),
              conditionalPanel(condition = "input.movebank_save_local == 1",
                textInput("movebank_local_filename", "Local filename")
              ),
            ),
            conditionalPanel(condition = "input.data_source === 'Your computer'",
              fileInput("local_file", label = NULL, multiple = FALSE,
                buttonLabel = "Browse",
                accept = c("text/csv","text/comma-separated-values,text/plain",
                           ".csv"
                )
              )
            ),
            conditionalPanel(
              condition = "input.data_source === 'Gateway' || input.data_source === 'Your computer'",
              radioButtons("coordinates", label = "Coordinate system:",
                choices = list("Latitude/Longitude" = 1, "UTM" = 2),
                selected = 1
              ),
              conditionalPanel(condition = "input.coordinates === '2'",
                # zone 30 is CA & AZ
                numericInput("zone", label = "Zone", value = 11, min = 1,
                  max = 60, step = 1, width = "50%"
                )
              ),
              radioButtons("datum", label = "Datum:",
                choices = list("NAD 27" = "NAD27", "NAD 83" = "NAD83",
                  "WGS 84" = "WGS84"
                ),
                selected = "WGS84"
              ),
            ),
            hr(style = "border-top: 2px solid grey;"),
            actionButton("data_load_btn", label = "Load data"),
            # actionButton("reset_data", "Reset data"),
          ),
          tabPanel(title = "MCP", value = "MCP", tags$br(),
            radioButtons("display", label = "Display",
                         choices = list("Polygons & Points", "Points only")),
            tags$strong(id = "mcp_zoom_label", "Display zoom (meters):"),
            bsTooltip(id = "mcp_zoom_label", placement = "right",
                      title = "Brownian Bridge buffer"),
            numericInput("mcp_zoom", label = NULL, value = 100, min = 0,
                         width = "50%"),
            hr(style = "border-top: 2px solid grey;"),
            actionButton("mcp_plot_btn", label = "Plot"),
          ),
          tabPanel(title = "MKDE", value = "MKDE", tags$br(),
            tags$strong(id = "modelabel", "Mode:"),
            bsTooltip(id = "modelabel", placement = "right",
              title = "Currently only 2D is supported but we are planning on adding 2.5 and 3D"
            ),
            radioButtons("mode", label = NULL, inline = TRUE,
              choices = list("2D" = '2D', "2.5D" = '25D', "3D" = '3D')
            ),
            tags$strong(id = "sig2obslabel", "sig2obs (meters):"),
            bsTooltip(id = "sig2obslabel", placement = "right",
              title = "Location error / variance"
            ),
            numericInput("sig2obs", label = NULL, value = 25.0, width = "50%"),
            tags$strong(id = "tmaxlabel", "Time max (minutes):"),
            bsTooltip(id = "tmaxlabel", placement = "right",
              title = "Maximum time threshold between consecutive locations"
            ),
            numericInput("tmax", label = NULL, value = 185.0, width = "50%"),
            #bsPopover(id = "tmax", title = "title", content = "content"),
            numericInput("cellsize", label = "Cell size (meters):", value = 0,
              min = 1, width = "75%"
            ),
            tags$strong(id = "mkde_buffer_label", "Buffer (meters):"),
            bsTooltip(id = "mkde_buffer_label", placement = "right",
              title = "Brownian Bridge buffer"
            ),
            numericInput("mkde_buffer", label = NULL, value = 1000.0,
              width = "50%"
            ),
            tags$strong(id = "probabilitylabel", "Cumulative probabilities:"),
            bsTooltip(id = "probabilitylabel", placement = "right",
              title = "Used to plot probability range; should be comma separated values"
            ),
            textInput("probability", label = NULL,
              value = "0.99, 0.95, 0.90, 0.75, 0.5, 0.0"
            ),
            # https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny
            div(style = "display: inline-block;vertical-align:sub;",
                checkboxInput("map", NULL, value=TRUE)),
            div(style = "display: inline-block;",
                tags$strong(id = "maplabel", "Show map background")),
            hr(style = "border-top: 2px solid grey;"),
            
            actionButton("mkde_plot_btn", label = "Plot"),
            actionButton("reset_parameters", "Reset parameters"),
            
            hr(style = "border-top: 4px solid black;"),
            
            checkboxInput("save_files", "Save output files..."),
            # radioButtons("save_files", label = "Save output files...",
            #              choices = list("Save" = 1)),
            conditionalPanel(
              condition = "input.save_files == 1",
              checkboxGroupInput("save_mkde_type", "Type:",
                                 inline = TRUE,
                                 c("Raster" = "raster", "Shape" = "shape")),
              radioButtons("save_mkde_data", label = "Data:",
                           choices = list("Current selected" = 'current',
                                          "All calculated" = 'calculated')),
              tags$strong(id = "save_raster_label", "Basename:"),
              bsTooltip(id = "save_raster_label", placement = "right",
                        title = "File name prefix"),
              textInput("basename", label = NULL),
              HTML(paste(tags$strong("Note:"),
                         "Files will be saved to your gateway home directory",
                         tags$span(style="color:red",
                                   "and existing file(s) will be overwritten!"))),
              hr(style = "border-top: 2px solid grey;"),
              actionButton("save_mkde_btn", label = "Save"),
            ),
            
            # tags$strong("Save output files..."),
            # tags$br(),
            
            # tags$strong(id = "save_raster_label", "Save raster file:"),
            # bsTooltip(id = "save_raster_label", placement = "right",
            #           title = "filename extension"
            # ),
            # checkboxInput("save_raster", "Save raster file"),
            # checkboxInput("save_shape", "Save 4 shape files"),
            # conditionalPanel(
            #   condition = "input.save_raster == 1 || input.save_shape == 1",
            #   textInput("basename", "Basename"),
            #   HTML(paste(tags$strong("Note:"),
            #              "Files will be saved to your gateway home directory",
            #              tags$span(style="color:red",
            #                        "and existing file(s) will be overwritten!"))),
            #                  # p("<b>Note:</b> Files will be saved to your gateway home directory"),
            # ),
          )
        ),
        # htmlOutput("status_log"),
        # From https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193
        # tags$head(tags$style("#status_log{color:red; font-size:12px; font-style:italic; overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))
        # tags$head(tags$style("#status_log{overflow-y:scroll; max-height: 50px;}"))
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        htmlOutput("instructions"),
        # https://github.com/daattali/shinycssloaders/
        # shinycssloaders::withSpinner(plotOutput("mcp_plot" ), type = 1),
        # shinycssloaders::withSpinner(plotOutput("mkde_plot" ), type = 4),
        plotOutput("mcp_plot" ),
        plotOutput("mkde_plot"),

        tabsetPanel(
          id = "tables",
          type = "tabs", 
          # header = tagList(h1("HEADER")),
          tabPanel("All", value = 1,
            shinycssloaders::withSpinner(DT::dataTableOutput('table_all'),
              type = 5
            )
          ),
          tabPanel("Summary", value = 2,
                   # Choose/update units for animal areas
                   # hr(style = "border-top: 1px solid #000000;"),
                   tags$div(id = "areaUnitsDiv",
                   tags$strong(id = "areaUnitsLabel", "Area units:"),
                   bsTooltip(id = "areaUnitsLabel", placement = "right",
                             title = "Sets units for area calculations"
                   ),
                   #tags$style(HTML("label{float:left;}")),
                   radioButtons("areaUnits", label = "",
                                choices = list("m2" = 'm2', "ha" = 'ha', "km2" = 'km2'),
                                selected = "ha", inline = TRUE
                   ),
                   ),
            shinycssloaders::withSpinner(DT::dataTableOutput('table_summary'),
              type = 6
            )
          )
        )
      )
    )
  )
)


# Define server logic required
server <- function(input, output, session) {
  
  # print(paste("options :", options()))
  # increase file uplaod size to 30 MB
  # (https://groups.google.com/g/shiny-discuss/c/rU3vwGMZexQ/m/zeKhiYXrtEQJ)
  options(shiny.maxRequestSize=100*1024^2)
  
  # print(paste("session$user:", session$user))
  #print(paste("session$userData:", session$userData))
  # print(paste("session$clientData:", session$clientData))
  pdf(file = NULL)

  # Global variables
  current_table_selection <- reactiveVal("single")
  gps <- reactiveValues()
  log <- reactiveVal(NULL)
  recalculate_raster <- reactiveVal(TRUE)
  replot_mkde <- reactiveVal(TRUE)
  
  # Utility function to append a message to the status_log component
  # display_log <- function(msg, br = TRUE) {
  #   tmp <- log()
  #   if(br) {
  #     log(paste(tmp, msg, '<br/>'))
  #   } else {
  #     log(paste(tmp, msg))
  #   }
  #   output$status_log <- renderUI({HTML(log())})
  # }
  
  
  # Initialize UI
  
  # Hide the plotting control tabs until data is loaded
  hideTab(inputId = "controls", target = "MCP")
  hideTab(inputId = "controls", target = "MKDE")

  output$instructions <- renderUI({
    tagList(h4("First, load your data and verify coordinate parameters..." ))}
  )
  
  # shinyjs::hide("status_log")
  shinyjs::hide("mcp_plot")
  shinyjs::hide("tables")
  shinyjs::hide("save_files")
  shinyjs::disable(selector = "[type=radio][value=25D]")
  shinyjs::disable(selector = "[type=radio][value=3D]")
  
  
  # When left side panel tab changes...
  observeEvent(input$controls, {
    #print(paste("** input$controls =", input$controls))
    if (input$controls == "Data") {
      if (isEmpty (gps$original)) {
        output$instructions <- renderUI({
          tagList(h4("Welcome, first step is to load your data..."),
                  tags$hr(style = "border-top: 2px solid #000000;")
          )
        })
      } else {
        output$instructions <- renderUI({
          tagList(h4("Load new data and/or change current data parameters..."),
                  tags$hr(style = "border-top: 2px solid #000000;")
          )
        })
        shinyjs::click("mkde_plot_btn")
      }
    } else if (input$controls == "MCP") {
      output$instructions <- renderUI({
        tagList(h4("To plot minimum convex polygon(s):"),
                tags$ol(tags$li("Set parameter (left)"),
                        tags$li("Choose one or more animals from Summary table (below)"),
                        tags$li("Plot")
                ),
                tags$hr(style = "border-top: 2px solid #000000;")
        )
      })
      shinyjs::hide("mkde_plot")
      shinyjs::show("mcp_plot")
      current_table_selection("multiple")
    } else if (input$controls == "MKDE") {
      shinyjs::click("mkde_plot_btn")
      output$instructions <- renderUI({
        tagList(h4("To plot movement-based kernel density estimator:"),
                tags$ol(tags$li("Set parameters (left)"),
                        tags$li("Choose an animal from Summary table (below)"),
                        tags$li("Plot (left bottom)")
                ),
                tags$hr(style = "border-top: 2px solid #000000;")
        )
      })
      shinyjs::hide("mcp_plot")
      shinyjs::show("mkde_plot")
      current_table_selection("single")
    }
  })
  
  # The following observe serves 2 purposes:
  # 1. any of the inputs involved with raster calculation changes, set flag to
  # clear rasters
  # 2. check parameters, if invalid will turn border to red, otherwise no color
  observe ({
    recalculate_raster(TRUE)
    replot_mkde(TRUE)
    
    if (!is.numeric (input$zone) || input$zone < 1 || input$zone > 60) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('zone').style.border ='", color,
                   "'"))
    
    if (!is.numeric (input$mcp_zoom) || input$mcp_zoom < 0) {
      color <- "solid #FF0000"
      shinyjs::disable("mcp_plot_btn")
    } else {
      color <- ""
      shinyjs::enable("mcp_plot_btn")
    }
    runjs (paste0 ("document.getElementById('mcp_zoom').style.border ='", color,
                   "'"))
    
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

    if (!is.numeric (input$mkde_buffer) || input$mkde_buffer < 0) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('mkde_buffer').style.border ='", color,
                   "'"))
    
    if (isEmpty(input$basename)) {
      color <- "solid #FF0000"
    } else {
      color <- ""
    }
    runjs (paste0 ("document.getElementById('basename').style.border ='", color,
                   "'"))
    
    if (isEmpty(input$basename) || isEmpty(input$save_mkde_type)) {
      shinyjs::disable("save_mkde_btn")
    } else {
      shinyjs::enable("save_mkde_btn")
    }
  })
  
  # The following observe serves 2 purposes:
  # 1. if probability changes, set flag to replot using MKDE
  # 2. check probability parameter, if invalid will turn border to red,
  # otherwise no color
  observeEvent(input$probability, {
    replot_mkde(TRUE)
    
    # following test doesn't seem to work for the entire probability string
    # regexpr("[:alpha:]", input$probability) != -1)
    if(isEmpty(input$probability) ||
       regexpr("[a-zA-Z]", input$probability) != -1) {
      color <- "solid #FF0000"
    } else {
      parts <- strsplit(input$probability, ",| |, ")
      color <- ""
      for(part in parts) {
        part_num <- as.double(part)
        #if(is.na(part_num) || part_num >= 1.0 || isZero(part_num)) {
        ifelse(part_num >= 1.0, color <- "solid #FF0000", "")
      }
    }
    
    runjs (paste0 ("document.getElementById('probability').style.border ='",
                   color, "'"))
  })
  
  # setup & display the gateway browser & selected file
  # volumes <- c (Home = fs::path_home(), "R Installation" = R.home(),
  #               getVolumes()())
  gateway_volumes <- c (Home = fs::path_home())
  shinyFileChoose (input, "gateway_browse", roots = gateway_volumes,
                   session = session)
  output$gateway_file <-
    renderUI ({
      if (is.integer (input$gateway_browse)) {
        HTML ("No file selected")
      } else {
        tmp <- parseFilePaths (gateway_volumes, input$gateway_browse)
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
        isEmpty (parseFilePaths (gateway_volumes, input$gateway_browse)$name) ||
        (input$data_source == 'Your computer' && isEmpty(input$local_file)) ||
        (input$data_source == 'Movebank' && (isEmpty(input$movebank_username) ||
                                             isEmpty(input$movebank_password) ||
                                             isEmpty(input$movebank_studyid)))) {
      shinyjs::disable("data_load_btn")
      # shinyjs::disable("reset_data")
    } else {
      shinyjs::enable("data_load_btn")
      # shinyjs::enable("reset_data")
    }
  })
  
  # If there is data and cell size is valid and a table row is selected, enable
  # Plot button
  observe ({
    if (! isEmpty (gps$original)  &&
       is.numeric(input$sig2obs)  && input$sig2obs >= 0 &&
       is.numeric(input$tmax)     && input$tmax >= 0 &&
       is.numeric(input$cellsize) && input$cellsize >= 1 &&
       is.numeric(input$mkde_buffer)   && input$mkde_buffer >= 0)
      shinyjs::enable("mkde_plot_btn")
    else
      shinyjs::disable("mkde_plot_btn")
  })
  
  # Handles user "Load data" button click...
  observeEvent(input$data_load_btn, {
  # eventReactive(input$data_load_btn, {
    
    # print("caught load data button event!")
    # print(paste("gps =", gps))
    # print(paste("gps$rasters =", gps$rasters))
    
    # shinyjs::show("status_log")

    # if there are rasters, then we need to clear the data and plot
    if (! is.null (gps$rasters)) {
      gps$data <<- NULL
      gps$original <<- NULL
      gps$rasters <<- NULL
      gps$summary <<- NULL
      shinyjs::show("instructions")
    }

    if (input$data_source == 'Gateway') {
      file <- parseFilePaths (gateway_volumes, input$gateway_browse)
      filename <- file$name
      id <- "load_gateway"
      message <- paste("Loading gateway file", filename, "...")
      showNotification(message, id = id, type = "message", duration = NULL,
                       session = session)
      results = loadDataframeFromFile (file$datapath)
      basename <- strsplit(filename, "\\.")[[1]]
      basename <- basename[1]
    } else if (input$data_source == 'Movebank') {
      filename <- input$movebank_studyid
      id <- "load_movebank"
      message <- "Accessing Movebank..."
      showNotification(message, duration = NULL, id = id, type = "message",
                       session = session)
      results <- loadDataframeFromMB(username = input$movebank_username,
                                     password = input$movebank_password,
                                     study = input$movebank_studyid)
      basename <- input$movebank_studyid
    } else if(input$data_source == 'Your computer') {
      # print(paste("input$local_file$name =", input$local_file$name))
      # print(paste("input$local_file$size =", input$local_file$size))
      # print(paste("input$local_file$type =", input$local_file$type))
      # print(paste("input$local_file$datapath =", input$local_file$datapath))
      filename <- input$local_file$name
      id <- "load_your_computer"
      message <- paste("Loading local file", filename, "...")
      showNotification(message, id = id, type = "message", duration = NULL,
                       session = session)
      results = loadDataframeFromFile(input$local_file$datapath)
      basename <- strsplit(input$local_file$name, "\\.")[[1]]
      basename <- basename[1]
    }
    
    data <- results[[1]]
    # print(paste("filename =", filename))
    # print(paste("id = ", id))
    # print(paste("data nrow =", nrow(data)))
    # print(paste("basename = ", basename))
    # print(paste("results[2] =", results[[2]]))
    if (length(results[[2]] > 0)) {
      removeNotification(id = id, session = session)
      showNotification(paste("Error", filename, ":", results[[2]]),
                       duration = NULL, type = "error", session = session)
    } else {
      showNotification(paste(message, "done"), duration = 2, id = id,
                       type = "message", session = session)
      updateTextInput(session, "basename", value = basename)
      
      rm(results)
      # printf(paste("loaded data # rows =", nrow(data), "; columns :"))
      # print(names(data))
  
      # display_log("* Preprocessing data...", FALSE)
      id <- "preprocessing"
      message <- "Preprocessing data..."
      showNotification(message, id = id, type = "message", duration = NULL,
                       session = session)
      results <- preprocessDataframe(data)
      showNotification(paste(message, "done"), id = id, type = "message", 
                       duration = 2, session = session)
      # display_log("done")
      shiny::validate(need(is.null(results[[2]]), results[[2]]))

      data <- results[[1]]
  
      gps$data <- results[[1]]
      gps$original <- results[[1]]
      
      # Now save MB data locally
      if(input$data_source == "Movebank" && input$movebank_save_local == 1) {
        # printf("Saving local file %s...", input$movebank_local_filename)
        result = saveDataframeFromMB(gps$original, input$movebank_local_filename)
        if(is.null(result))
          printf("done\n")
        else
          printf("error: %s\n", result)
      }
      
      continue <- TRUE
      
      # printf("Calculating spatial attributes...")
      # display_log("* Calculating spatial attributes...", FALSE)
      id <- "attributes"
      message <- "Calculating spatial attributes..."
      tryCatch({
        showNotification(message, id = id, type = "message", duration = NULL,
                         session = session)
        data <- animalAttributes(data, input$areaUnits)
        showNotification(paste(message, "done"), id = id, type = "message",
                         duration = 2, session = session)
        # printf("done\n")
        # display_log("done")
      },
      error = function(e) {
        # print(paste("error = ", e$message))
        showNotification(paste("Error :", e$message), id = id,
                         duration = NULL, type = "error", session = session)
        continue <<- FALSE
      })
      gps$summary <- data
      shinyjs::show("tables")

      if (continue) {
        # Upate UI elements
        showTab(inputId = "controls", target = "MCP", session = session)
        showTab(inputId = "controls", target = "MKDE", session = session)
        output$instructions <- renderUI(
          tagList(h4("Next, select the plot type you want..."),
                  tags$ul(tags$li("MCP = Minimum Convex Polygon"),
                          tags$li("MKDE = Movement-based Kernel Density Estimator")
                  ),
                  # tags$br(),
                  # tags$br(),
                  # tags$br(),
                  tags$hr(style = "border-top: 2px solid #000000;")
          )
        )
        updateTabsetPanel(session, "tables", selected = "2")
        # shinyjs::show("tables")
      }
    }
  })
  
  
  # Tables
  
  # Change table_all_data when input$control changes between MCP and MKDE
  # code from https://stackoverflow.com/a/34590704/1769758
  # table_all_data <- eventReactive (gps$original, {
  # table_all_data <- eventReactive(list(gps$original, input$controls), {
  table_all_data <- reactive({
    DT::datatable(
      gps$original[], extensions = 'Buttons',
      #caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
      options = list(
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        #paging = FALSE, scrollX = TRUE, scrollY = TRUE, 
        stateSave = TRUE), 
      selection = list(mode = current_table_selection(), target = 'row'))
  })
  
  # table_summary_data <- eventReactive (gps$summary, {
  table_summary_data <- reactive({
    shinyjs::show ("tables")

    DT::datatable (
      gps$summary[], extension = "Buttons",
      #caption="You can do multi-column sorting by shift clicking the columns\n\nm = meters",
      #caption = tagList(h1("Caption")),
      options = list(
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        stateSave = TRUE),
      selection = list(mode = current_table_selection(), selected = 1, target = 'row'))
  })
  
  output$table_all <- DT::renderDataTable(table_all_data())
  output$table_summary <- DT::renderDataTable(table_summary_data())
  
  observeEvent(input$areaUnits, {
    if (!is.null(gps$original)) {
      id <- "recalc"
      message <- "Re-calculating spatial attributes..."
      showNotification(message, id = id, type = "message", duration = NULL,
                       session = session)
      data <- animalAttributes(gps$original, input$areaUnits)
      # printf(paste("data =", data, "\n"))
      showNotification(paste(message, "done"), duration = 2, id = id,
                       type = "message", session = session)
      gps$summary <- data
    }
  })
  
  
  plot_mcp <- eventReactive(input$mcp_plot_btn, {
    printf("plot_mcp!\n")
    shinyjs::hide("instructions")
    data <- gps$data
    summary <- gps$summary
    id <- summary$id[input$table_summary_rows_selected]
    mode <- TRUE
    if (input$display == "Points only")
      mode <- FALSE
    map <- minConvexPolygon(data, input$zone, input$datum, input$mcp_zoom, id,
                            mode)
    map
  })
  
  plot_mkde <- eventReactive(input$mkde_plot_btn, {
    # printf("plot_mkde!\n")
    shinyjs::hide("instructions")
    # shinyjs::disable("inputs")
    shinyjs::disable("controls")

    summary <- gps$summary
    id <- summary$id[input$table_summary_rows_selected]
    raster <- NULL
    rasters <- gps$rasters
    if(recalculate_raster())
      rasters <- NULL
    
    if (! is.null (rasters) && ! is.null (rasters[[id]])) {
      raster <- rasters[[id]]$raster
    } else {
      data <- gps$data
      
      # display_log("* Calculating raster...", FALSE)
      # print("calculateRaster2D()...")
      raster <- calculateRaster2D(data, id, input$sig2obs, input$tmax,
                                  input$cellsize, input$mkde_buffer)
      # display_log("done")
      recalculate_raster(FALSE)
      # print(paste("raster class =", class(raster)))
      # print(paste("nrow =", raster::nrow(raster), "; ncol =",
      #             raster::ncol(raster), "; ncell =", raster::ncell(raster),
      #             "dim =", dim(raster)))
      # print(paste("columns :", names(raster)))
      # print(paste("rasters id =", id))
      rasters[[id]]$raster <- raster
      rasters[[id]]$id <- id
      # print(paste("rasters length =", length(rasters)))
      gps$rasters <- rasters
      # print(paste("gps$rasters length =", length(gps$rasters)))

      # Now show the save output UI
      shinyjs::show("save_files")
    }
    
    # print(paste("replot_mkde =", replot_mkde()))
    if(replot_mkde()) {
      tryCatch({
        probs = as.numeric ( unlist (strsplit (input$probability, ",")))
        # print(paste("save_mkde_type:", input$save_mkde_type))
        # createContour(raster, probs, input$zone, input$datum, raster, shape,
        #               input$basename)
        # results <- createContour(raster, probs, input$zone, input$datum, TRUE, TRUE,
        #               input$basename)
        results <- createContour(raster, probs, input$map)
        
        # can't assign to gps$rasters directly
        rasters <- gps$rasters
        rasters[[id]]$contours <- results
        gps$rasters <- rasters
      },
      error = function(error_message) {
        print(paste("error message =", error_message))
        shiny::validate(need(error_message == "",
                             "Unable to plot; please try adjusting the parameter(s) and Plot again..."))
      },
      finally = {
        # shinyjs::enable("inputs")
        shinyjs::enable("controls")
      })
    }
    # shinyjs::enable("inputs")
  })

  output$mcp_plot <- renderPlot({
    # printf("render mcp_plot\n")
    plot_mcp()
  })
  
  output$mkde_plot <- renderPlot({
    #printf("render mkde_plot\n")
    replot_mkde(TRUE)
    plot_mkde()
    #printf("leaving mkde_plot\n")
  })
  
  # observeEvent ( input$reset_data, {
  #   shinyjs::reset("data_source")
  #   shinyjs::reset ( "local_file" )
  #   shinyjs::reset ( "movebank_username" )
  #   shinyjs::reset ( "movebank_password" )
  #   shinyjs::reset ( "movebank_studyid" )
  #   shinyjs::reset ( "movebank_save_local" )
  #   shinyjs::reset("coordinates")
  #   shinyjs::reset("zone")
  #   shinyjs::reset("datum")
  #   output$table_all <- DT::renderDataTable(NULL)
  #   output$table_summary <- DT::renderDataTable(NULL)
  #   shinyjs::disable("data_load_btn")
  #   shinyjs::disable("reset_data")
  #   gps <- reactiveValues()
  # } )
  
  observeEvent(input$reset_parameters, {
    shinyjs::reset("sig2obs")
    shinyjs::reset("tmax")
    shinyjs::reset("cellsize")
    shinyjs::reset("mkde_buffer")
    shinyjs::reset("probability")
  })
  
  observeEvent(input$save_mkde_btn, {
    # print("save mkde output button!")
    # print(paste("save_mkde_data =", input$save_mkde_data))
    id <- NULL
    if (input$save_mkde_data == "current") {
      summary <- gps$summary
      id <- summary$id[input$table_summary_rows_selected]
    }
    # print(paste("idx =", id))
    
    mid <- "save_output"
    message <- paste("Saving output files...")
    showNotification(message, id = mid, type = "message", duration = NULL,
                     session = session)
    save_output(input$save_mkde_type, gps$rasters, id, input$zone, input$datum,
                input$basename)
    showNotification(paste(message, "done"), id = mid, type = "message",
                     duration = 2, session = session)
  })
}

shinyApp ( ui = ui, server = server )
