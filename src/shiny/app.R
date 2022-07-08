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

source("gps.R")
source("movebank.R")
source("util.R")

sessionInfo()

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

      tabsetPanel(type = "tabs",
                  tabPanel("1. Load Data",
                           hr(style = "border-top: 1px solid #000000;"),
                           fileInput("file.upload",
                                     "OR upload your GPS data file:",
                                     multiple = FALSE,
                                     accept = c ( "text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv"))),
                  textInput("movebank.username", "Movebank Username",
                                     value = "mona", width = NULL,
                                     placeholder = NULL),
                           passwordInput("movebank.password", "Movebank Password",
                                         value = "g0MB2022", width = NULL,
                                         placeholder = NULL),
                           textInput("movebank.studyid", "Movebank Study ID",
                                       value = "408181528", width = NULL,
                                       placeholder = NULL),
                  tabPanel("2. Set Parameters",
                           numericInput("sig2obs", label = h4("sig2obs"),
                                        value = 25.0),
                           numericInput("tmax", label = h4("t.max (minutes)"),
                                        value = 185.0),
                           numericInput("cellsize", label = h4("cell size (meters)"),
                                        value = 30 ),
                           # disable https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
                           radioButtons("radio", label = h4("mode"),
                                        choices = list("2D" = 2, "2.5D" = 1, "3D" = 3), 
                                        selected = 2))),
      hr(style = "border-top: 2px solid #000000;"),
      actionButton("runx", label = "Run"),
      actionButton("reset", "Reset form"),

	    textOutput ( "debug" ),
	  
	    verbatimTextOutput ( "file_value" ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # https://github.com/daattali/shinycssloaders/
      shinycssloaders::withSpinner(plotOutput ( "mkdePlot" ), type = 5),
      #plotOutput ( "mkdePlot" ),
      textOutput("table.info"),
      #htmlOutput("table.info"),
      DT::dataTableOutput('table')
    ),
  )
)


# Define server logic required
server <- function ( input, output, session ) {

  shinyjs::disable("radio")
  
  #output$status <- renderPrint({"Please load your data from either MoveBank or browse to a local file..."})
  
  # If no file selected, disable Run button...
  observe ( {
    if ( ( is.null ( input$file.upload ) || input$file.upload == "" ) &&
         ( is.null ( input$movebank.username ) || input$movebank.username == "" )
         && ( is.null ( input$movebank.password ) ||
              input$movebank.password == "" ) &&
         ( is.null ( input$movebank.studyid ) || input$movebank.studyid == "" ) ) {
      shinyjs::disable ( "runx" )
      shinyjs::disable ( "reset" )
    } else {
      shinyjs::enable ( "runx" )
      shinyjs::enable ( "reset" )
    }
  } )
  
  #movebank.data <- eventReactive ( input$runx, {
  #  print("run event 1")
  #})

  mkde.plot <- eventReactive ( input$runx, {
    if ( ! is.null ( input$movebank.username ) && input$movebank.username != "" &&
         ! is.null ( input$movebank.password ) &&
         ! is.null ( input$movebank.studyid ) ) {
      shinyjs::disable ( "runx" )

      #data <- getMovebankData ( study=strtoi ( input$movebank.studyid ), login=login )
      #output$status <- renderPrint({"Retrieving data from MoveBank..."})
      print("Accessing Movebank...")
      #withProgress(message = "Retrieving data from MoveBank...", {
      results <-
        movebankDataLoader(username = input$movebank.username,
                           password = input$movebank.password,
                           study = input$movebank.studyid, login = login )
      #})
      print("Done")
      shiny::validate(need(results[[2]] == "", results[[2]]))
      data <- results[[1]]
      errors <- results[[2]]

      #output$status <- renderPrint({"Saving data locally..."})
      
      print("Creating table...")
      stat <- animalAttributes(data)
      #print(paste("stat =", stat))
      
      caption <- "m = meters"
      
      #exampletext <- rep(as.list("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), 5)
      #output$table.info <- renderUI(lapply(exampletext, tags$p))
      
      output$table.info <- renderText({"The following table gives the extent of animal movement for the individuals and for the data set as a whole, along with the grid dimensions resulting from various pixel sizes. Keep in mind that larger grids result in longer calculations, so you may want to choose a pixel size that result in a smaller grid for preliminary calculations.\n E-W(m) and N-S(m) are the east-west and north-south ranges, in meters px(m) is the pixel size in meters and grid is the resulting grid dimensions\n"})
      
      #diamonds2 = diamonds[sample(nrow(diamonds), 5), ]
      #print(paste("class =", class(diamonds2)))
      #print(paste("str =", str(diamonds2)))
      #print(paste("diamonds2 =", diamonds2))
      output$table <- DT::renderDataTable({
        DT::datatable(stat[], extensions = 'Buttons', caption=caption,
                      options = list(autoWidth = TRUE, dom = 'Bfrtip',
                                     buttons = c('csv', 'excel')),
                      selection = list(mode = 'single', selected = c(1), target = 'row'))
      })
      
      
      print("Creating plot...")
      ##withProgress(message = "Creating plot...", {
        ##plotMKDE ( movebankProcess ( input$sig2obs, input$tmax, data ) )
      #plots <- movebankProcess(input$sig2obs, input$tmax, data)
      #plotMKDE(plots[[1]])
      ##})
      #print("Plotting done")
      shinyjs::enable ( "runx" )
    }
    else if(! is.null(input$file.upload)) {
      plotMKDE(GPSDataLoader(input$sig2obs, input$tmax, input$cellsize,
                             input$file.upload$datapath))}
  })
  
  output$mkdePlot <- renderPlot ( { mkde.plot() } )
  
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

