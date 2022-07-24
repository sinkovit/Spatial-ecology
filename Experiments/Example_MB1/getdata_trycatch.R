data_loader <- function(study, login) {
    tryCatch(
        {
        d <- getMovebankData(study=study, login=login)
	return(list(d, ""))
        },
        error = function(error_message) {
            errmsg1 = "It looks like you are not allowed to download this data set, have you agreed to the license terms in the web interface?"
            if(error_message[1] == errmsg1) {
               msg <- paste(errmsg1, ": Go to Movebank and accept license terms")
               return(list(NA, msg))
	    }
            errmsg2 = "unable to find an inherited method for function ‘getMovebankData’ for signature ‘\"numeric\", \"NULL\", \"MovebankLogin\"’"
            if(error_message[1] == errmsg2) {
               msg <- paste(errmsg2, ": Appears to be invalide Movebank study ID")
               return(list(NA, msg))
	    }
        }
    )
}

results <- data_loader(study, login)
data <- results[[1]]
errors <- results[[2]]
