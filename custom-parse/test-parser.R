source("parser.R")

# Test script for parser code

infiles <- c("Data/T-x-Y-z.txt",
"Data/TIME-x-Y-z.csv",
"Data/Y-Time-X-Z.txt",
"Data/t-lat-long-id.csv",
"Data/t-x-y.txt",
"Data/t_timestamp-x-y-z.csv",
"Data/time-x-y.txt",
"Data/timestamp-lat-long-id.csv",
"Data/missing-lat.csv",
"Data/missing-t.txt",
"Data/missing-x.txt",
"Data/date-x-y.txt")

for (file in infiles) {
    print(file)
    gpsdata <- parseGpsText(file)
    print(head(gpsdata))
    cat('\n')
}
