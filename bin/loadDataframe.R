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

  if (ext != "csv" && ext != "txt") {
    return(list(NULL,
                paste("Unknown file extension .", ext,
                      "; only files with .csv or .txt are accepted", sep = "")))
  } else if (ext == "csv") {
    gpsdata <- read.csv(file, header = TRUE)
    # gpsdata <- read.csv(file, header = TRUE, row.names = 1)
  } else if (ext == "txt") {
    gpsdata <- read.table(file, header=TRUE)
  }
  # print(paste("gpsdata :", summary(gpsdata)))
  # print(paste("gpsdata length =", length(gpsdata)))
  # print(paste("gpsdata nrow =", nrow(gpsdata)))
  # print(paste("gpsdata colnames =", colnames(gpsdata)))
  
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
loadDataframeFromMB <- function(study, username, password, remove_outliers) {
  # print(paste("username =", username))
  # print(paste("password =", password))
  # print(paste("study = ", study))
  # print(paste("remove_outliers = ", remove_outliers))
  file.local <- paste(getwd(), "/Study-", toString(study), ".RData", sep="")
  #print(paste("file.local =", file.local))
  key <- keyring::key_list(service="movebank")
  # print(paste("key =", key, "\n"))
  if (nrow(key) == 0) {
    print("no key!\n")
    movebank_store_credentials(username, password)
    print("stored mb credential\n")
  } else {
    print("key found!\n")
  }

  tryCatch({
    # # if(file.exists(file.local)) {
    # #   printf("  data exists locally, loading...")
    # #   load(file.local)
    # #   printf("done\n")
    # #   return(list(data, NULL))
    # # } else {
    # # maybe try shiny::invalidateLater()?
    # # save(data, file=file.local)
    # data <- movebank_download_study(study_id=strtoi(study),
    # attributes = c("location_long", "location_lat",
    #                "timestamp"))
    # remove_movebank_outliers=remove_outliers)
    data <- movebank_retrieve(entity_type = 'event', study_id = strtoi(study),
                              remove_movebank_outliers=remove_outliers)
    print("post data")
    # converting tibble to data.frame
    results <- as.data.frame(data)
    return(list(results, "Your Movebank account info has been saved"))
  },
  error = function(error_message) {
    print(paste("error message =", error_message))
    # message(rlang::last_error()$error)
    # rlang::last_error()$url
    
    if (str_detect(error_message[1], "you are not allowed to download")) {
      # Movebank data license url =
      # https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study<study_id>
      return(list(NULL,
                  "Please go to Movebank and accept the data license terms, then return here and try again..."))
    } else if (str_detect(error_message[1],
                   "unable to find an inherited method for function ‘getMovebankLocationData’")) {
      return(list(NULL, "Invalid Movebank study ID. Do you have access to this dataset?  Did you agree to the licensing term on Movebank.org?  Is it a test study?"))
    } else if (str_detect(error_message[1], "There are no valid credentials")) {
      return(list(NULL,
                  "Invalid login credential. Please check your username and password or go to Movebank.org and verify your account is valid."))
    } else if (str_detect(error_message[1], "No data are available for download")) {
      return(list(NULL, "No data available for download."))
    } else if (str_detect(error_message[1], "Timeout was reached")) {
      return(list(NULL,
                  "Movebank.org timed out. Please wait a few minutes and try again..."))
    } else if (str_detect(error_message[1], "Empty reply from server")) {
      return(list(NULL,
                  "Empty reply from movebank.org server. Please wait a few minutes and try again..."))
    } else if (str_detect(error_message[1], "too close to the limit")) {
      return(list(NULL, paste("System error (", error_message[1],
                              ").  Please wait a few minutes and try again...")))
    }
    return(list(NULL, error_message))
  })
}


# Save the data frame to a local file
# param data - data frame
# param filename - local file name
# return NULL if no problem; otherwise return a user-friendly error message
saveDataframe <- function(data, filename) {
  if(!is.data.frame(data))
    return("Error: invalid data frame!")
  
  if(isEmpty(filename))
    return("Error: invalid filename!")
  
  write.csv(data, filename, row.names = FALSE)
  return(NULL)
}

