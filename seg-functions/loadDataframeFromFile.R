loadDataframeFromFile <- function(file) {

# Parse a plain text (whitespace separate values) or csv file
# containing biotelemetry data

      library(tools)

      ext <- file_ext(file)
      if (ext == "csv") { 
      	 gpsdata <- read.csv(file, header=TRUE)
      } else if (ext == "txt") {
      	 gpsdata <- read.table(file, header=TRUE)
      } else { 
      	print("Unknown file extension")
      	# implement error handling
      }

      return(gpsdata)
}
