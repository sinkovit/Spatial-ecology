library ( shiny )
library ( mkde )
library ( raster )
library ( R.utils )
library ( shinyjs )

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

#if ( interactive() ) {
#  print ( "interactive!" )
#} else {
#  print ( "not interactive!" )
#}

# Define UI for app that draws a histogram ----
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

	  # Input: Select a file ----
	  fileInput ( "file.input", "Please upload your GPS data file:",
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

	  textOutput ( "selected_var" ),
	  
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

# Define server logic required to draw a histogram ----
server <- function ( input, output, session ) {

  # If no file selected, disable Run button...
  observe ( {
    if ( is.null ( input$file.input ) || input$file.input == "" ) {
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
  
  #output$selected_var <- renderText ( {"Running..."} )
  
  #output$selected_var <- renderPrint ( { input$file.input } )
  #output$file_value <- renderPrint ( { str ( input$file.input$datapath ) } )
    
    #observeEvent ( input$runx, { output$mkdePlot <- renderPlot ( { plotMKDE (
    #  getMKDEData ( input$sig2obs, input$tmax, input$file.input$datapath ) ) } ) } )

  mkde.plot <- eventReactive ( input$runx, { plotMKDE (
    getMKDEData ( input$sig2obs, input$tmax, input$file.input$datapath ) )  } )
  output$mkdePlot <- renderPlot ( { mkde.plot() } )

  # Reset sig2obs and t.mat; unfortunately can't "reset" input file
  # (see https://stackoverflow.com/questions/44203728/how-to-reset-a-value-of-fileinput-in-shiny
  # for a trick)
  observeEvent ( input$reset, {
    shinyjs::reset ( "myapp" )
  } )
  
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

