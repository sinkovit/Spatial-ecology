loadDataframeFromMB <- function(study, username, password) {

# Load data from Movebank, return gpsdata and error message

  library(move)
      
  login <- movebankLogin(username=username, password=password)
  errmsg <- ""

  tryCatch(
     {
     gpsdata <- getMovebankLocationData(study=study, login=login)
     return(list(gpsdata, errmsg))
     },
     error = function(error_message) {
	if(grepl("It looks like you are not allowed to download", error_message[1], ignore.case = TRUE)) {
            errmsg <- paste("You are not allowed to download study ", study, ". Go to Movebank and accept license terms", sep="")
            return(list(NA, errmsg))
        }
	if(grepl("unable to find an inherited method for function", error_message[1], ignore.case = TRUE)) {
            errmsg <- paste("Study", toString(study), "appears to be an invalid Movebank study ID")
            return(list(NA, errmsg))
        }
     }
   )
}
