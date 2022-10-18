library(move)
source("loadDataframeFromMB.R")
source("preprocessDataframe.R")
username <- "RSinkovits"
password <- "aBBa&0805&mb"
study <- 408181528
login <- movebankLogin(username=username, password=password)
gpsdata <- getMovebankLocationData(study=study, login=login)
gpsdata <- preprocessDataframe(gpsdata)
