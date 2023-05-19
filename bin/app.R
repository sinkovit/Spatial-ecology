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

source("compute.R")
source("loadDataframe.R")
# source("minConvexPolygon.R")
# source("plotDataframe.R")
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
            radioButtons("data_source", "Load data from :",
              choices = c ("Gateway", "Movebank", "Your computer")
            ),
            conditionalPanel(condition = "input.data_source === 'Gateway'",
              fluidRow(
                column(width = 4, offset = 0,
                  shinyFilesButton('gateway_browse', label='Browse',
                    title = 'Select your GPS data file', multiple = FALSE,
                    viewtype = "detail"
                  )
                ),
                column(width = 8, offset = 0, htmlOutput ("gateway_file"))
              ),
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
                accept = c("text/csv", "text/comma-separated-values,text/plain",
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
            hr(style = "border-top: 2px solid #000000;"),
            actionButton("data_load_btn", label = "Load data"),
            actionButton("reset_data", "Reset data"),
          ),
          tabPanel(title = "MCP", value = "MCP", tags$br(),
            radioButtons("display", label = "Display",
                         choices = list("Polygon", "Points")),
            tags$strong(id = "mcp_buffer_label", "Buffer (meters):"),
            bsTooltip(id = "mcp_buffer_label", placement = "right",
                      title = "Brownian Bridge buffer"),
            numericInput("mcp_buffer", label = NULL, value = 100, min = 0,
                         width = "50%"),
            hr(style = "border-top: 2px solid #000000;"),
            actionButton("mcp_plot_btn", label = "Plot"),
          ),
          tabPanel(title = "MKDE", value = "MKDE", tags$br(),
            tags$strong(id = "modelabel", "Mode:"),
            bsTooltip(id = "modelabel", placement = "right",
              title = "Currently only 2D is supported but we are planning on adding 2.5 and 3D"
            ),
            radioButtons("mode", label = NULL,
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
            hr(style = "border-top: 2px solid #000000;"),
            actionButton("mkde_plot_btn", label = "Plot"),
            actionButton("reset_parameters", "Reset parameters"),
            
            # Choose/update units for animal areas
            hr(style = "border-top: 2px solid #000000;"),
            tags$strong(id = "areaUnitsLabel", "Area units:"),
            bsTooltip(id = "areaUnitsLabel", placement = "right",
              title = "Sets units for area calculations"
            ),
            radioButtons("areaUnits", label = NULL,
              choices = list ("m2" = 'm2', "ha" = 'ha', "km2" = 'km2'),
              selected = "ha", inline = TRUE
            ),
            actionButton("updateUnits", label = "Update area units")
          )
        ),
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        htmlOutput("instructions"),
        # https://github.com/daattali/shinycssloaders/
        # shinycssloaders::withSpinner(plotOutput("mcp_plot" ), type = 1),
        shinycssloaders::withSpinner(plotOutput("mkde_plot" ), type = 4),
        plotOutput("mcp_plot" ),
        # plotOutput("mkde_plot"),

        tabsetPanel(id = "tables", type = "pills",
          tabPanel("All", value = 1,
            shinycssloaders::withSpinner(DT::dataTableOutput('table_all'),
              type = 5
            )
          ),
          tabPanel("Summary", value = 2,
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
  
  # clear_plot <- reactiveVal(FALSE)
  current_table_selection <- reactiveVal("single")
  gps <- reactiveValues()
  recalculate_raster <- reactiveVal(TRUE)
  replot_mkde <- reactiveVal(TRUE)
  
  # Hide the plotting control tabs until data is loaded
  hideTab(inputId = "controls", target = "MCP")
  hideTab(inputId = "controls", target = "MKDE")

  output$instructions <- renderUI({
    tagList(h4("First, load your data and verify coordinate parameters..." ))}
  )
  
  shinyjs::hide("mcp_plot")
  # shinyjs::hide("mkde_plot")
  shinyjs::hide("tables")
  shinyjs::disable(selector = "[type=radio][value=25D]")
  shinyjs::disable(selector = "[type=radio][value=3D]")
  
  # When left side panel tab changes...
  observeEvent(input$controls, {
    print(paste("** input$controls =", input$controls))
    #if (input$controls == "Data" && ! isEmpty (gps$original)) {
    if (input$controls == "Data") {
      output$instructions <- renderUI({
        tagList(h4("Load new data and/or change current data parameters..."),
                tags$hr(style = "border-top: 2px solid #000000;")
        )
        # shinyjs::hide("mcp_plot")
        # shinyjs::hide("mkde_plot")
      })
    } else if (input$controls == "MCP") {
      output$instructions <- renderUI({
        tagList(h4("To plot minimum convex polygon(s):"),
                tags$ol(tags$li("Set parameter (left)"),
                        tags$li("Choose animal(s) from Summary table (below)"),
                        tags$li("Plot")
                ),
                tags$hr(style = "border-top: 2px solid #000000;")
        )
      })
      shinyjs::hide("mkde_plot")
      shinyjs::show("mcp_plot")
      # shinyjs::disable("mkde_plot")
      # shinyjs::enable("mcp_p")
      current_table_selection("multiple")
    } else if (input$controls == "MKDE") {
      printf("here 4\n")
      output$instructions <- renderUI({
        tagList(h4("To plot movement-based kernel density estimator:"),
                tags$ol(tags$li("Set parameters (left)"),
                        tags$li("Choose an animal from Summary table (below)"),
                        tags$li("Plot (left bottom)")
                ),
                tags$hr(style = "border-top: 2px solid #000000;")
        )
      })
      #plotOutput("mkde_plot" )
      shinyjs::hide("mcp_plot")
      shinyjs::show("mkde_plot")
      # shinyjs::disable("mcp_plot")
      # shinyjs::enable("mkde_plot")
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
    
    if (!is.numeric (input$mcp_buffer) || input$mcp_buffer < 0) {
      color <- "solid #FF0000"
      shinyjs::disable("mcp_plot_btn")
    } else {
      color <- ""
      shinyjs::enable("mcp_plot_btn")
    }
    runjs (paste0 ("document.getElementById('mcp_buffer').style.border ='", color,
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
      shinyjs::disable("reset_data")
    } else {
      shinyjs::enable("data_load_btn")
      shinyjs::enable("reset_data")
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
    
    # if there are rasters, then we need to clear the data and plot
    if (! is.null (gps$rasters)) {
      # clear_plot (TRUE)
      gps$data <- NULL
      gps$original <- NULL
      gps$rasters <- NULL
      gps$summary <- NULL
      shinyjs::show("instructions")
    }

    if(input$data_source == 'Gateway') {
      file <- parseFilePaths (gateway_volumes, input$gateway_browse)
      printf ("Loading gateway file %s...", file$name)
      results = loadDataframeFromFile (file$datapath)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))
      printf("done\n")
      data = results[[1]]
    } else if(input$data_source == 'Movebank') {
      printf("Accessing Movebank...\n")
      results <- loadDataframeFromMB(username = input$movebank_username,
                                     password = input$movebank_password,
                                     study = input$movebank_studyid)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))

      data = results[[1]]
    } else if(input$data_source == 'Your computer') {
      printf("Loading local file %s...", input$local_file$name)
      results = loadDataframeFromFile(input$local_file$datapath)
      shiny::validate(need(is.null(results[[2]]), results[[2]]))
      printf("done\n")
      data = results[[1]]
    }
    
    rm(results)
    # printf(paste("loaded data # rows =", nrow(data), "; columns :"))
    # print(names(data))

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
    
    printf("Calculating spatial attributes...")
    data <- animalAttributes(data, input$areaUnits)
    printf("done\n")

    gps$summary <- data
    
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
    shinyjs::show("tables")
  })
  
  # table_all <- reactive({
  #   
  # })
  
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
      options = list(
        autoWidth = TRUE, buttons = c('csv', 'excel'), dom = 'Bfrtip',
        pagingType = "full_numbers", processing = TRUE, scrollX = TRUE,
        stateSave = TRUE),
      selection = list(mode = current_table_selection(), selected = 1, target = 'row'))
  })
  
  output$table_all <- DT::renderDataTable(table_all_data())
  output$table_summary <- DT::renderDataTable(table_summary_data())
  
  plot_mcp <- eventReactive(input$mcp_plot_btn, {
    printf("plot_mcp!\n")
    data <- gps$data
    summary <- gps$summary
    id <- summary$id[input$table_summary_rows_selected]
    mode <- TRUE
    if (input$display == "Points")
      mode <- FALSE
    map <- minConvexPolygon(data, input$zone, input$datum, input$mcp_buffer, id, mode)
    map
  })
  
  plot_mkde <- eventReactive(input$mkde_plot_btn, {
    printf("mkde plot button event!\n")
    shinyjs::hide("instructions")
    shinyjs::disable("inputs")

    summary <- gps$summary
    id <- summary$id[input$table_summary_rows_selected]
    raster <- NULL
    rasters <- gps$rasters
    if(recalculate_raster())
      rasters <- NULL
    
    if (! is.null (rasters) && ! is.null (rasters[[id]])) {
      raster <- rasters[[id]]
    } else {
      data <- gps$data
      
      # Spatial extent can be calculated in different ways, for example from
      # data set itself, from digital elevation model or manually set. For
      # now, just using min/max values for the GPS readings.
      xmin <- min(data$xdata) - input$mkde_buffer
      xmax <- max(data$xdata) + input$mkde_buffer
      ymin <- min(data$ydata) - input$mkde_buffer
      ymax <- max(data$ydata) + input$mkde_buffer
      
      print(paste("input$zone =", input$zone))
      print(paste("input$datum =", input$datum))
      print(paste("id =", id))
      raster <- calculateRaster2D (data, id, input$sig2obs, input$tmax,
                                   input$cellsize, xmin, xmax, ymin, ymax)
      recalculate_raster(FALSE)
      #raster <- minConvexPolygon(data, 11, "WGS84", id, TRUE)
      #raster <- minConvexPolygon(data, input$zone, input$datum, id, TRUE)
      # print(paste("raster class =", class(raster)))
      # print(paste("nrow =", raster::nrow(raster), "; ncol =",
      #             raster::ncol(raster), "; ncell =", raster::ncell(raster),
      #             "dim =", dim(raster)))
      # print(paste("columns :", names(raster)))
      rasters[[id]] <- raster
      gps$rasters <- rasters
    }
    
    print(paste("replot_mkde =", replot_mkde()))
    if(replot_mkde()) {
      tryCatch({
        probs = as.numeric ( unlist (strsplit (input$probability, ",")))
        # plotMKDE(raster, probs = probs, asp = raster$ny/raster$nx, xlab = '',
        #          ylab = '')
        createContour(raster, probs, "tmp", input$zone, input$datum)
      },
      error = function(error_message) {
        print(paste("error message =", error_message))
        shiny::validate(need(error_message == "",
                             "Unable to plot; please try adjusting the parameter(s) and Plot again..."))
      })
    }
    shinyjs::enable("inputs")
  })

  output$mcp_plot <- renderPlot({
    printf("render mcp_plot\n")
    if (input$controls == "MCP") {
      plot_mcp()
    # } else {
    #   return(NULL)
    }
  })
  
  output$mkde_plot <- renderPlot({
    printf("render mkde_plot\n")
    printf(paste("input$controls =", input$controls, "\n"))
    # print(paste("clear_plot =", clear_plot()))
    # if (clear_plot()) {
    #   printf("clear plot\n")
    #   clear_plot(FALSE)
    #   return()
    # } else {
    if (input$controls == "MKDE") {
      printf("here 1\n")
      # shinyjs::hide("mcp_plot")
      # shinyjs::show("mkde_plot")
      plot_mkde()
    # } else {
    #   return(NULL)
    }
    printf("leaving mkde_plot\n")
  })
  
  observeEvent ( input$reset_data, {
    shinyjs::reset("data_source")
    shinyjs::reset ( "local_file" )
    shinyjs::reset ( "movebank_username" )
    shinyjs::reset ( "movebank_password" )
    shinyjs::reset ( "movebank_studyid" )
    shinyjs::reset ( "movebank_save_local" )
    shinyjs::reset("coordinates")
    shinyjs::reset("zone")
    shinyjs::reset("datum")
    #shinyjs::reset("table_all")
    # output$table_all <- DT::renderDataTable(NULL)
    # output$table_summary <- DT::renderDataTable(NULL)
    # following 3 lines don't work to clear the tables...
    # shinyjs::reset("table_all")
    # shinyjs::reset("table_summary")
    # table_all_data <- DT::datatable(NULL)
    shinyjs::disable("data_load_btn")
    shinyjs::disable("reset_data")
    gps <- reactiveValues()
  } )
  
  observeEvent(input$reset_parameters, {
    shinyjs::reset("sig2obs")
    shinyjs::reset("tmax")
    shinyjs::reset("cellsize")
    shinyjs::reset("mkde_buffer")
    shinyjs::reset("probability")
  })

  observeEvent ( input$updateUnits, {
    printf(paste("Units updated to", input$areaUnits, "\n"))
    data <- animalAttributes(gps$original, input$areaUnits)
    gps$summary <- data
  } )
}

shinyApp ( ui = ui, server = server )
