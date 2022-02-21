library ( shiny )
library ( mkde )
library ( raster )
library ( R.utils )
sessionInfo()

displaymkde <- function ( sig2obs, tmax, path.file ) {
  # Read GPS movement data from file
  print ( "entered displaymkde" )

  #if ( ! ( is.character ( path.file ) && length ( path.file ) == 1 ) ) {
  #  print ( "file is null!" )
  #  return ()
  #}
  
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

if ( interactive() ) {
  print ( "interactive!" )
} else {
  print ( "not interactive!" )
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # tags$head(tags$script(src = "message-handler.js")),
  
  # App title ----
  titlePanel("Welcome to the Spatial Ecology Gateway!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

	  # Input: Select a file ----
	  fileInput ( "file.input", "Please upload your GPS data file:",
                multiple = FALSE,
                accept = c ( "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

	  # disable https://stackoverflow.com/questions/58310378/disable-single-radio-choice-from-grouped-radio-action-buttons
	  radioButtons("radio", label = h3("Mode"),
    			   choices = list("2D" = 2, "3D" = 3), 
    			   selected = 2),

	  # Copy the line below to make a number input box into the UI.
	  numericInput("sig2obs", label = h3("sig2obs"), value = 25.0),

	  # Copy the line below to make a number input box into the UI.
	  numericInput("tmax", label = h3("t.max"), value = 185.0),

	  actionButton("run", label = "Run"),

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

      #plotOutput ( outputId = "mkdePlot" ),
      plotOutput ( "mkdePlot" ),
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot"),

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  #observeEvent(input$run, {
  #  session$sendCustomMessage(type = 'testmessage',
  #                            message = 'Thank you for clicking')
  #})
  
  #output$selected_var <- renderText ( {"Running..."} )
  
  #if ( ! ( is.character ( input$file_var.datapath ) && length ( input$file_var.datapath ) == 1 ) ) {
    output$selected_var <- renderPrint ( { input$sig2obs } )
    output$file_value <- renderPrint ( { str ( input$file.input$datapath ) } )
    #output$file_value <- renderTable({
    #  file <- input$file.input
    #  ext <- tools::file_ext(file$datapath)
      
    #  req(file)
    #  validate(need(ext == "txt", "Please upload a .txt file"))
      
    #  read.csv(file$datapath, header = input$header)
    #})

    observeEvent ( input$run, { output$mkdePlot <- renderPlot ( { plotMKDE (
      displaymkde ( input$sig2obs, input$tmax, input$file.input$datapath ) ) } ) } )
    
    #mkde <- eventReactive ( input$run, { runif ( input$sig2obs ) } )
  #}

  
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

