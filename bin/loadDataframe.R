# This software is Copyright 2022 The Regents of the University of California.
# All Rights Reserved. Permission to copy, modify, and distribute this software
# and its documentation for educational, research and non-profit purposes,
# without fee, and without a written agreement is hereby granted, provided that
# this entire copyright appear in all copies. Permission to make commercial use
# of this software may be obtained by contacting:
# 
# Office of Innovation and Commercialization
# 9500 Gilman Drive, Mail Code 0910
# University of California
# La Jolla, CA 92093-0910
# (858) 534-5815
# invent@ucsd.edu
#
# This software program and documentation are copyrighted by The Regents of the
# University of California. The software program and documentation are supplied
# “as is”, without any accompanying services from The Regents. The Regents does
# not warrant that the operation of the program will be uninterrupted or
# error-free. The end-user understands that the program was developed for
# research purposes and is advised not to rely exclusively on the program for
# any reason.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
# LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION,
# EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE. THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED
# HEREUNDER IS ON AN “AS IS” BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO
# OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.

# Parse a plain text (whitespace separate values) or csv file containing
# biotelemetry data
# Returns a list where first item is the data and second item is the error
# message; if successful, the error message = NULL and data will be
# populated; if there is an error message, then data = NULL

loadDataframeFromFile <- function(file) {
  ext <- file_ext(file)
  # print(paste("ext =", ext))
  
  if (ext != "csv" && ext != "txt") {
    return(list(NULL,
                paste("Unknown file extension .", ext,
                      "; only files with .csv or .txt are accepted", sep = "")))
  }
  else if (ext == "csv") { 
    gpsdata <- read.csv(file, header=TRUE)
  } else if (ext == "txt") {
    gpsdata <- read.table(file, header=TRUE)
  }
  # print(paste("gpsdata :", gpsdata))
  # print(paste("length =", length(gpsdata)))
  # print(paste("nrow =", nrow(gpsdata)))
  
  if (base::nrow(gpsdata) < 1) {
    return(list(NULL, paste("No data found in file")))
  } else {
    return(list(gpsdata, NULL))
  }
}


# Load data from Movebank, return gpsdata and error message
# Returns a list where first item is the data and second item is the error
# message; if successful, there error message = NULL and data will be
# populated; if there is an error message, then data = NULL
loadDataframeFromMB <- function(study, username, password) {
  # print(paste("username =", username))
  # print(paste("password =", password))
  # print(paste("study = ", study))
  file.local <- paste(getwd(), "/Study-", toString(study), ".RData", sep="")
  #print(paste("file.local =", file.local))
  
  tryCatch({
    # if(file.exists(file.local)) {
    #   printf("  data exists locally, loading...")
    #   load(file.local)
    #   printf("done\n")
    #   return(list(data, NULL))
    # } else {
    login <- movebankLogin(username = username, password = password )
    # maybe try shiny::invalidateLater()?
    # data <- getMovebankData(study=strtoi(study), login=login)
    data <- getMovebankLocationData(study=strtoi(study), login=login)
    # save(data, file=file.local)
    return(list(data, NULL))
    # }
  },
  error = function(error_message) {
    # print(paste("error message =", error_message))
    if (str_detect(error_message[1], "you are not allowed to download")) {
      # Movebank data license url =
      # https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study<study_id>
      return(list(NULL,
                  "Please go to Movebank and accept the data license terms, then return here and try again..."))
    }
    if (str_detect(error_message[1],
                   "unable to find an inherited method for function ‘getMovebankLocationData’")) {
      return(list(NULL, "Invalid Movebank study ID. Do you have access to this dataset?  Did you agree to the licensing term on Movebank.org?  Is it a test study?"))
    }
    if (str_detect(error_message[1], "There are no valid credentials")) {
      return(list(NULL,
                  "Invalid login credential. Please check your username and password or go to Movebank.org and verify your account is valid."))
    }
    if (str_detect(error_message[1], "No data are available for download")) {
      return(list(NULL, "No data available for download."))
    }
    if (str_detect(error_message[1], "Timeout was reached")) {
      return(list(NULL,
                  "Movebank.org timed out. Please wait a few minutes and try again..."))
    }
    if (str_detect(error_message[1], "Empty reply from server")) {
      return(list(NULL,
                  "Empty reply from movebank.org server. Please wait a few minutes and try again..."))
    }
    return(list(NULL, error_message))
  })
}


# Save the data frame to a local file
# param data - data frame
# param filename - local file name
# return NULL if no problem; otherwise return a user-friendly error message
saveDataframeFromMB <- function(data, filename) {
  if(!is.data.frame(data))
    return("Invalid data frame to save!")
  
  if(isEmpty(filename))
    return("Invalid filename!")
  
  write.csv(data, filename)
  return(NULL)
}

