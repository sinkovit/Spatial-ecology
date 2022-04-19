library ( shiny )
library ( mkde )
library ( raster )
library ( R.utils )
library ( shinyjs )
library ( move )

sessionInfo()

getMKDEData <- function ( sig2obs, tmax, path.file ) {
  # Read GPS movement data from file
  #print ( "entered getMKDEData" )

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
  
  # Note to Mona - the value that	you read from the Shiny	app will replace
  # Note to Mona - the hardcoded values (25.0 and 185.0) assigned	to the	
  # Note to Mona - variables sig2obs and t.max
  # Note to Mona - this is exactly what you had done in previous examples
  
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
    
    Sys.sleep(5)
    
    # Create home range using mkde
    mv.dat <- initializeMovementData(t, x, y, sig2obs=sig2obs, t.max=tmax)
    mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
    dens.res <- initializeDensity(mkde.obj, mv.dat)
    mkde.obj <- dens.res$mkde.obj
    mv.dat <- dens.res$move.dat
    return ( mkde.obj )
  }
}

#if ( interactive() ) {
#  print ( "interactive!" )
#} else {
#  print ( "not interactive!" )
#}

# Define UI for app
ui <- fluidPage(
  useShinyjs(), # include shinyjs

  # tags$head(tags$script(src = "message-handler.js")),
  
  # App title ----
  titlePanel ( "Welcome to the Spatial Ecology Gateway!" ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout (

    # Sidebar panel for inputs ----
    sidebarPanel (
      
    id = "myapp",

    textInput ( "movebank.username", "Movebank Username", value = "",
                width = NULL, placeholder = NULL),
    passwordInput ( "movebank.password", "Movebank Password", value = "",
                width = NULL, placeholder = NULL),
    textInput ( "movebank.studyid", "Movebank Study ID", value = "",
                width = NULL, placeholder = NULL),
    # 1989169785 = Baja Golden eagle
    # 408181528 = California Condor
    
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
	  
      # Input: Slider for the number of bins ----
      #sliderInput(inputId = "bins",
                  #label = "Number of bins:",
                  #min = 1,
                  #max = 50,
                  #value = 30),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      plotOutput ( "mkdePlot" ),
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot"),

    )
  )
)

# Define server logic required
server <- function ( input, output, session ) {

  movebank.outfile <- paste ( "movebank.data" )
  
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
  #observeEvent(input$runx, {
  #  session$sendCustomMessage(type = 'testmessage',
  #                            message = 'Thank you for clicking')
  #})
  
  output$debug <- renderText ( { "Running..." } )
  
  #output$selected_var <- renderPrint ( { input$file.upload } )
  #output$file_value <- renderPrint ( { str ( input$file.upload$datapath ) } )
    
    #observeEvent ( input$runx, { output$mkdePlot <- renderPlot ( { plotMKDE (
    #  getMKDEData ( input$sig2obs, input$tmax, input$file.upload$datapath ) ) } ) } )

  #mkde.plot <- eventReactive ( input$runx, { plotMKDE (
  #  getMKDEData ( input$sig2obs, input$tmax, input$file.upload$datapath ) ) } )
  #output$mkdePlot <- renderPlot ( { mkde.plot() } )

  mkde.plot <- eventReactive ( input$runx, {
    if ( ! is.null ( input$movebank.username ) && input$movebank.username != "" &&
         ! is.null ( input$movebank.password ) &&
         ! is.null ( input$movebank.studyid ) ) {
      output$debug <- renderText ( "Movebank!" )
      login <- movebankLogin ( username=input$movebank.username,
                               password=input$movebank.password )
      data <- getMovebankData ( study=input$movebank.studyid, login=login )
      #save ( data, file=movebank.outfile )
      mb2 ( data )
      plotMKDE ( mb2 ( input$sig2obs, input$tmax, data ) )
    }
    else if ( ! is.null ( input$file.upload ) ) {
      output$debug <- renderText ( { "upload file" } )
      plotMKDE ( getMKDEData ( input$sig2obs, input$tmax,
                               input$file.upload$datapath ) ) } } )
  output$mkdePlot <- renderPlot ( { mkde.plot() } )
  
  # Reset sig2obs and t.mat; unfortunately can't "reset" input file
  # (see https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny
  # for a trick)
  observeEvent ( input$reset, {
    shinyjs::reset ( "myapp" )
    #input$file.upload = ""
    #hide ( "mkdePlot" )
    #output$mkdePlot <- NULL
    shinyjs::disable ( "runx" )
    shinyjs::disable ( "reset" )
  } )
  
  #observeEvent ( input$runx, {
  #  show ( "mkdePlot" )
  # } )
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  #output$distPlot <- renderPlot({

    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    #hist(x, breaks = bins, col = "#75AADB", border = "white",
         #xlab = "Waiting time to next eruption (in mins)",
         #main = "Histogram of waiting times")

    #})
  
}

shinyApp ( ui = ui, server = server )

