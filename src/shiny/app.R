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
# loaded. The animal_attribute() function is first defined and call to
# function is made at bottom of file

# --------------------------------------------------------------------

animal_attributes <- function(data_df) {
  
  # Replicate C printf functionality
  printf <- function(...) cat(sprintf(...))
  
  # Custom rounding to 2 significant digits if x > 10, 1 digit otherwise
  custom_round <- function(x) {return (signif(x, min(2,floor(log10(x)))))}
  
  animals <- append(as.list(sort(unique(data_df$local_identifier))), "all")
  max_pixels <- c(50, 100, 200, 500)
  
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
    
    for (max_pix in max_pixels) {
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
    }
    printf("\n")
  }
}

# Calling the function; assumes data_df is already defined
#animal_attributes(data_df)

# https://github.com/sinkovit/Spatial-ecology/blob/Bob/Example_MB1/getdata_trycatch.R
data_loader <- function(username, password, study, login) {
  tryCatch(
    {
      print("Authenticating into Movebank...")
      login <- movebankLogin(username = username, password = password )
      print("Retrieving data from Movebank...")
      d <- getMovebankData(study=strtoi(study), login=login)
      return(list(d, ""))
    },
    error = function(error_message) {
      if(str_detect(error_message[1], "you are not allowed to download")) {
        # Movebank data license url =
        # https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study<study_id>
        return(list(NULL,
                    "Please go to Movebank and accept the data license terms, then return here and try again."))
      }
      if(str_detect(error_message[1],
                    "unable to find an inherited method for function ‘getMovebankData’")) {
        return(list(NULL, paste("Error:", study,
                                "appears to be an invalid Movebank study ID.")))
      }
      if(str_detect(error_message[1], "There are no valid credentials")) {
        return(list(NULL,"Error: invalid login credential. Please check your username and password or go to Movebank.org and verify your account is valid."))
      }
      if(str_detect(error_message[1], "No data are available for download")) {
        return(list(NULL,"Error: no data available for download."))
      }
      return(list(NULL, error_message))
    }
  )
}


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


mb2 <- function(sig2obs, tmax, data) {
  # Data preparation
  # (1) Convert lat/long to aeqd (Azimuthal Equidistance) projection
  # (2) Convert MoveStack to data frame
  # (3) Convert timestamps to epoch minutes
  printf("Processing data...")
  data <- spTransform(data, center=TRUE)
  data_df <- as.data.frame(data)
  data_df$time = as.numeric(as.POSIXct(data_df$timestamp)) / 60
  local_identifiers <- unique(data_df$local_identifier)
  #print(paste("local_identifiers = ", local_identifiers))
  #print(paste("length = ", length(local_identifiers)))
  plots <- list()

  for (local_id in local_identifiers) {
    x <- data_df[which(data_df$local_identifier == local_id), "location_long.1"]
    y <- data_df[which(data_df$local_identifier == local_id), "location_lat.1"]
    t <- data_df[which(data_df$local_identifier == local_id), "time"]
    
    # Get data range; set grid size and cell size
    xmin <- min(x)
    ymin <- min(y)
    xmax <- max(x)
    ymax <- max(y)
    xrange <- xmax-xmin
    yrange <- ymax-ymin
    
    if (xrange >= yrange) {
      nx <- 50
      ny <- as.integer(nx * (yrange/xrange))
      cell.sz <- xrange/nx
    } else {
      ny <- 50
      nx <- as.integer(ny * (xrange/yrange))
      cell.sz <- yrange/ny
    }  
    
    print(paste("local_identifier =", local_id))
    print("Home range dimensions (pixels/voxels)")
    print(paste("nx =", nx, "ny =", ny))
    print("Home range dimensions (meters)")
    print(paste("xrange =", xrange, "yrange =", yrange))
    print(paste("cell size =", cell.sz))
    
    # Create home range using mkde
    mv.dat <- initializeMovementData(t, x, y, sig2obs=sig2obs, t.max=tmax)
    mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
    dens.res <- initializeDensity(mkde.obj, mv.dat)
    mkde.obj <- dens.res$mkde.obj
    mv.dat <- dens.res$move.dat
    plots <- append(plots, list(mkde.obj))
  }
  return(plots)
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

      textInput ( "movebank.username", "Movebank Username", value = "",
                  width = NULL, placeholder = NULL ),
      passwordInput ( "movebank.password", "Movebank Password", value = "",
                  width = NULL, placeholder = NULL),
      textInput ( "movebank.studyid", "Movebank Study ID", value = "",
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
      DT::dataTableOutput('table')
    ),
  )
)


# Define server logic required
server <- function ( input, output, session ) {

  movebank.outfile <- paste ( "movebank.data" )
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

  mkde.plot <- eventReactive ( input$runx, {
    if ( ! is.null ( input$movebank.username ) && input$movebank.username != "" &&
         ! is.null ( input$movebank.password ) &&
         ! is.null ( input$movebank.studyid ) ) {
      
      #data <- getMovebankData ( study=strtoi ( input$movebank.studyid ), login=login )
      #output$status <- renderPrint({"Retrieving data from MoveBank..."})
      print("Accessing Movebank...")
      #withProgress(message = "Retrieving data from MoveBank...", {
      results <- data_loader(username = input$movebank.username,
                             password = input$movebank.password,
                             study = input$movebank.studyid, login = login )
      #})
      print("Done")
      shiny::validate(need(results[[2]] == "", results[[2]]))
      data <- results[[1]]
      errors <- results[[2]]

      #output$status <- renderPrint({"Saving data locally..."})
      #save ( data, file=movebank.outfile )
      shinyjs::disable ( "runx" )
      print("Creating plot...")
      #withProgress(message = "Creating plot...", {
        #plotMKDE ( mb2 ( input$sig2obs, input$tmax, data ) )
      plots <- mb2(input$sig2obs, input$tmax, data)
      plotMKDE(plots[[1]])
      #})
      print("Plotting done")
      shinyjs::enable ( "runx" )
    }
    else if ( ! is.null ( input$file.upload ) ) {
      plotMKDE ( getMKDEData ( input$sig2obs, input$tmax,
                               input$file.upload$datapath ) ) }
  } )
  
  output$mkdePlot <- renderPlot ( { mkde.plot() } )
  
  diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  #print(paste("diamonds2 =", diamonds2[]))
  output$table <- DT::renderDataTable({
    DT::datatable(diamonds2[])
  })
  
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

