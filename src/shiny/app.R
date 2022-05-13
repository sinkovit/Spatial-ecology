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

# https://github.com/sinkovit/Spatial-ecology/blob/Bob/Example_MB1/getdata_trycatch.R
data_loader <- function(username, password, study, login) {
  tryCatch(
    {
      login <- movebankLogin(username = username, password = password )
      d <- getMovebankData(study=strtoi(study), login=login)
      return(list(d, ""))
    },
    error = function(error_message) {
      if(str_detect(error_message[1], "you are not allowed to download")) {
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


mb2 <- function ( sig2obs, tmax, data ) {
  # Data preparation
  # (1) Convert lat/long to aeqd (Azimuthal Equidistance) projection
  # (2) Convert MoveStack to data frame
  # (3) Convert timestamps to epoch minutes
  data <- spTransform(data, center=TRUE)
  data_df <- as.data.frame(data)
  data_df$time = as.numeric(as.POSIXct(data_df$timestamp)) / 60
  #print ( paste ( "data_df = ", data_df ) )
  
  for (local_id in unique(data_df$local_identifier)) {
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
      nx <- 500
      ny <- as.integer(nx * (yrange/xrange))
      cell.sz <- xrange/nx
    } else {
      ny <- 500
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
    return ( mkde.obj )
  }
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

  titlePanel ( "Welcome to the Spatial Ecology Gateway!" ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout (

    # Sidebar panel for inputs ----
    sidebarPanel (
      id = "myapp",

      textInput ( "movebank.username", "Movebank Username", value = "",
                  width = NULL, placeholder = NULL ),
      passwordInput ( "movebank.password", "Movebank Password", value = "!",
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
      
      #wellPanel(
      #  verbatimTextOutput("status"),
      #),
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
        plotMKDE ( mb2 ( input$sig2obs, input$tmax, data ) )
      #})
      print("Plotting done")
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

