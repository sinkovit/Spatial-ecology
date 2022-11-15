# Read data set and prepare data frame

library(shiny)

source("loadDataframeFromFile.R")
source("preprocessDataframe.R")

file <- "Data/CondorFull.csv"
#file <- "Data/pandabob.txt"

# Read and pre-process data
gpsdata <- loadDataframeFromFile(file)
gpsdata <- preprocessDataframe(gpsdata)
