library ( shiny )

# Simple example
#ui <- fluidPage ( "Hello World" )
#server <- function ( input, output ) {}


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Welcome to the Spatial Ecology Gateway!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

	  # Input: Select a file ----
	  fileInput("file1", "Please upload your GPS data file:",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

	  radioButtons("radio", label = h3("Mode"),
    			   choices = list("2D" = 2, "3D" = 3), 
    			   selected = 3),

	  # Copy the line below to make a number input box into the UI.
	  numericInput("sig2obs", label = h3("sig2obs"), value = 25.0),

	  # Copy the line below to make a number input box into the UI.
	  numericInput("tmax", label = h3("t.max"), value = 185.0),

	  actionButton("run", label = "Run"),

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

    })

}

shinyApp ( ui = ui, server = server )

