# This software is Copyright © 2022 The Regents of the University of California.
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

library(move)
library(tools)

# Parse a plain text (whitespace separate values) or csv file containing
# biotelemetry data
# Return a list where first item is the data and second item is the error
# message; if successful, there error message = NULL and data will be
# populated; if there is an error message, then data = NULL
loadDataFrameFromFile <- function(file) {
  #print(paste("loadDataFrameFromFile() file =", file))
  ext <- file_ext(file)
  if (ext == "csv") { 
    gpsdata <- read.csv(file, header=TRUE)
  } else if (ext == "txt") {
    gpsdata <- read.table(file, header=TRUE)
  } else {
    return(list(NULL,
                paste("Unknown file extension .", ext,
                      "; only files with .csv or .txt are accepted", sep = "")))
  }
  
  return(list(gpsdata, NULL))
}


loadDataFrameFromMB <- function(study, username, password) {
  
  # Load data from Movebank, return gpsdata and error message
  
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

