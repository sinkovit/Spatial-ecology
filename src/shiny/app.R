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

source("movebank.R")

sessionInfo()

# From Bob's code
# @ https://github.com/sinkovit/Spatial-ecology/blob/Bob/code-snippets/animal_stats.R
# The following code snippet shows how to list the stats for each
# animal and for the data set as a whole. It also provides advice on
# choosing pixel size.

# data_df is the data frame containg data that has already been
# transformed so that location_[lat|long].1 contain the azimuthal
# equidistance projection and t is time in minutes relative to the
# first observation in the entire data set

# Note 1 - I'm using the R range() function for both performance and
# to maintain more compact code. It returns a vector of min and max
# values

# Note 2 - code is written assuming that data has already been
# loaded. The animalAttributes() function is first defined and call to
# function is made at bottom of file

# --------------------------------------------------------------------

animalAttributes <- function(data_df) {
  
  # Replicate C printf functionality
  printf <- function(...) cat(sprintf(...))
  
  # Custom rounding to 2 significant digits if x > 10, 1 digit otherwise
  custom_round <- function(x) {return (signif(x, min(2,floor(log10(x)))))}
  
  animals <- append(as.list(sort(unique(data_df$local_identifier))), "all")
  print(paste("animals =", animals))
  max_pixels <- c(50, 100, 200, 500)
  print(paste("max_pixels =", max_pixels))
  
  printf("\n")
  printf("The following table gives the extent of animal movement for the\n")
  printf("individuals and for the data set as a whole, along with the grid\n")
  printf("dimensions resulting from various pixel sizes. Keep in mind that larger\n")
  printf("grids result in longer calculations, so you may want to choose a pixel\n")
  printf("size that result in a smaller grid for preliminary calculations.\n")
  printf("\n")
  printf("E-W(m) and N-S(m) are the east-west and north-south ranges, in meters\n")
  printf("px(m) is the pixel size in meters and grid is the resulting grid dimensions\n")
  printf("\n")
  
  printf("%7s ", "id")
  printf("%9s %9s %9s %9s %10s %10s ", "long_min", "long_max", "lat_min", "lat_max", "E-W(m)  ", "N-S(m)  ")
  for (max_pix in max_pixels) {
    printf("%6s %7s ", "px(m)", "grid")
  }
  printf("\n------- --------- --------- --------- --------- ---------- ---------- ------ ------- ")
  printf("------ ------- ------ ------- ------ -------\n")
  
  #result <- data.frame(id = numeric(), 'long min' = numeric(),
  #                     long_max = numeric(), lat_min = numeric(),
  #                     lat_max = numeric(), E_W = numeric(),
  #                     N_S = numeric())
  #, px(m) = numeric(), grid = character(),
  #                     px(m) = numeric(), grid = character(), px(m) = numeric(),
  #                     grid = character(), px(m) = numeric(), grid = character() )
  # Oddly above doesn't work but below works...
  result <- data.frame(id = numeric())
  result[ , 'long min'] <- numeric()
  result[ , 'long max'] <- numeric()
  result[ , 'lat min'] <- numeric()
  result[ , 'lat max'] <- numeric()
  result[ , 'E-W (m)'] <- numeric()
  result[ , 'N-S (m)'] <- numeric()
  row.index <- 1
  
  for (local_id in animals) {
    if(local_id == "all") {
      long_minmax = range(data_df$location_long)
      lat_minmax = range(data_df$location_lat)
      x_minmax = range(data_df$location_long.1)
      y_minmax = range(data_df$location_lat.1)
      t_minmax = range(data_df$time)
      printf("    all ")
    } else {
      long_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_long"])
      lat_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_lat"])
      x_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_long.1"])
      y_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_lat.1"])
      t_minmax = range(data_df[which(data_df$local_identifier == local_id), "time"])
      printf("%7i ", local_id)
    }
    x_range <- x_minmax[2] - x_minmax[1]
    y_range <- y_minmax[2] - y_minmax[1]
    printf("%9.4f %9.4f %9.4f %9.4f %10.2f %10.2f ", long_minmax[1], long_minmax[2], lat_minmax[1], lat_minmax[2], x_range, y_range)
    row <- c(local_id, round(long_minmax[1],3), round(long_minmax[2],3),
             round(lat_minmax[1],3), round(lat_minmax[2],3), round(x_range,3),
             round(y_range,3))
    #result[row.index, ] <- row
    print(paste("row head =", row))
    
    row.tail = c()
    
    for (max_pix in max_pixels) {
      print(paste("max_pix =", max_pix))
      if (x_range >= y_range) {
        nx <- max_pix
        ny <- as.integer(nx * (y_range/x_range))
        cell.sz <- x_range/nx
        cell.sz <- custom_round(cell.sz)
        nx <- as.integer(x_range/cell.sz)
        ny <- as.integer(y_range/cell.sz)
      } else {
        ny <- max_pix
        nx <- as.integer(ny * (x_range/y_range))
        cell.sz <- y_range/ny
        cell.sz <- custom_round(cell.sz)
        nx <- as.integer(x_range/cell.sz)
        ny <- as.integer(y_range/cell.sz)
      }
      dims <- sprintf("%dx%d", nx, ny)
      printf("%6.1f %7s ", cell.sz, dims)
      
      if(row.index == 1) {
        columns.new <- c('cell size', 'dimensions')
        print(paste("columns.new = ", columns.new))
        result[ , 'cell size'] <- numeric()
        result[ , 'dimensions'] <- character()
        print("here 1")
        str(result)
      }
      
      row.tail <- append(row.tail, c(cell.sz, dims))
      #print(paste("row.tail =", row.tail))
      #row <- list(row, row.tail)
      #row <- append(row, row.tail)
      #print(paste("added row = ", row))
    }
    print(paste("all row.tail = ", row.tail))
    row <- append(row,row.tail)
    print(paste("new row =", row))
    result[row.index, ] <- row
    row.index <- row.index + 1
    
    printf("\n")
  }
  return(result)
}

# Calling the function; assumes data_df is already defined
#animalAttributes(data_df)


getMKDEData <- function ( sig2obs, tmax, path.file ) {
  # Read GPS movement data from file
  
  panda <- read.table ( path.file, header=TRUE)
  
  xmin <- min(panda$x)
  ymin <- min(panda$y)
  xmax <- max(panda$x)
  ymax <- max(panda$y)
  xrange <- xmax-xmin
  yrange <- ymax-ymin
  cell.sz <- 10
  nx = xrange/cell.sz
  ny = yrange/cell.sz
  mv.dat <- initializeMovementData(panda$time, panda$x, panda$y, sig2obs=sig2obs, t.max=tmax)
  mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
  dens.res <- initializeDensity(mkde.obj, mv.dat)
  
  mkde.obj <- dens.res$mkde.obj
  mv.dat <- dens.res$move.dat
  return ( mkde.obj )
}

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

      textInput ( "movebank.username", "Movebank Username", value = "mona",
                  width = NULL, placeholder = NULL ),
      passwordInput ( "movebank.password", "Movebank Password", value = "g0MB2022",
                  width = NULL, placeholder = NULL),
      textInput ( "movebank.studyid", "Movebank Study ID", value = "408181528",
                  width = NULL, placeholder = NULL ),

      hr ( style = "border-top: 1px solid #000000;" ),
    
	    # Input: Select a file ----
	    fileInput ( "file.upload", "Please upload your GPS data file:",
                  multiple = FALSE,
                 accept = c ( "text/csv",
                          "text/comma-separated-values,text/plain",
                         ".csv")),

	    # disable https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
	    radioButtons ( "radio", label = h4 ( "Mode" ),
    			    choices = list ( "2D" = 2, "3D" = 3 ), 
    			    selected = 2 ),

	    # Copy the line below to make a number input box into the UI.
	    numericInput ( "sig2obs", label = h4 ( "sig2obs" ), value = 25.0 ),

	    # Copy the line below to make a number input box into the UI.
	    numericInput ( "tmax", label = h3("t.max"), value = 185.0 ),

	    actionButton ( "runx", label = "Run" ),
	    actionButton ( "reset", "Reset form" ),

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
    print("run event 2")
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
      print(paste("stat =", stat))
      
      caption <- "m = units in meters"
      
      #exampletext <- rep(as.list("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."), 5)
      #output$table.info <- renderUI(lapply(exampletext, tags$p))
      
      output$table.info <- renderText({"The following table gives the extent of animal movement for the individuals and for the data set as a whole, along with the grid dimensions resulting from various pixel sizes. Keep in mind that larger grids result in longer calculations, so you may want to choose a pixel size that result in a smaller grid for preliminary calculations.\n E-W(m) and N-S(m) are the east-west and north-south ranges, in meters px(m) is the pixel size in meters and grid is the resulting grid dimensions\n"})
      
      #diamonds2 = diamonds[sample(nrow(diamonds), 5), ]
      #print(paste("class =", class(diamonds2)))
      #print(paste("str =", str(diamonds2)))
      #print(paste("diamonds2 =", diamonds2))
      output$table <- DT::renderDataTable({
        DT::datatable(stat[], caption=caption)
      })
      
      
      #print("Creating plot...")
      ##withProgress(message = "Creating plot...", {
        ##plotMKDE ( mb2 ( input$sig2obs, input$tmax, data ) )
      #plots <- mb2(input$sig2obs, input$tmax, data)
      #plotMKDE(plots[[1]])
      ##})
      #print("Plotting done")
      shinyjs::enable ( "runx" )
    }
    else if ( ! is.null ( input$file.upload ) ) {
      plotMKDE ( getMKDEData ( input$sig2obs, input$tmax,
                               input$file.upload$datapath ) ) }
  } )
  
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

