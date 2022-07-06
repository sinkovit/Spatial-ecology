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

# https://github.com/sinkovit/Spatial-ecology/blob/Bob/Example_MB1/getdata_trycatch.R
movebankDataLoader <- function(username, password, study, login) {
  
  file.local <-
    paste(getwd(), "/Study-", toString(study), ".RData", sep="")
  #print(paste("file.local =", file.local))
  
  tryCatch(
    {
      if(file.exists(file.local)) {
        print("Data exists locally, loading...")
        load(file.local)
        return(list(data, ""))
      } else {
        print("Authenticating into Movebank...")
        login <- movebankLogin(username = username, password = password )
        print("Retrieving data from Movebank...")
        d <- getMovebankData(study=strtoi(study), login=login)
        print("Saving data locally...")
        save ( data, file=file.local )
        return(list(d, ""))
      }},
    error = function(error_message) {
      #print(paste("error_message =", error_message))
      if(str_detect(error_message[1], "you are not allowed to download")) {
        # Movebank data license url =
        # https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study<study_id>
        return(list(NULL,
                    "Please go to Movebank and accept the data license terms, then return here and try again."))
      }
      if(str_detect(error_message[1],
                    "unable to find an inherited method for function ‘getMovebankData’")) {
        return(list(NULL, paste("Error:", study,
                                "appears to be an invalid Movebank study ID.")))
      }
      if(str_detect(error_message[1], "There are no valid credentials")) {
        return(list(NULL,
                    "Error: invalid login credential. Please check your username and password or go to Movebank.org and verify your account is valid."))
      }
      if(str_detect(error_message[1], "No data are available for download")) {
        return(list(NULL, "Error: no data available for download."))
      }
      if(str_detect(error_message[1], "Timeout was reached")) {
        return(list(NULL,
                    "Error: movebank.org timed out. Please wait a few minutes and try again..."))
      }
      return(list(NULL, error_message))
    }
  )
}


movebankProcess <- function(sig2obs, tmax, data) {
  # Data preparation
  # (1) Convert lat/long to aeqd (Azimuthal Equidistance) projection
  # (2) Convert MoveStack to data frame
  # (3) Convert timestamps to epoch minutes
  printf("Processing data...")
  data <- spTransform(data, center=TRUE)
  data_df <- as.data.frame(data)
  data_df$time = as.numeric(as.POSIXct(data_df$timestamp)) / 60
  local_identifiers <- unique(data_df$local_identifier)
  #print(paste("local_identifiers = ", local_identifiers))
  #print(paste("length = ", length(local_identifiers)))
  plots <- list()
  
  for (local_id in local_identifiers) {
    x <- data_df[which(data_df$local_identifier == local_id), "location_long.1"]
    y <- data_df[which(data_df$local_identifier == local_id), "location_lat.1"]
    t <- data_df[which(data_df$local_identifier == local_id), "time"]
    
    # Get data range; set grid size and cell size
    xmin <- min(x)
    ymin <- min(y)
    xmax <- max(x)
    ymax <- max(y)
    xrange <- xmax-xmin
    yrange <- ymax-ymin
    
    if (xrange >= yrange) {
      nx <- 50
      ny <- as.integer(nx * (yrange/xrange))
      cell.sz <- xrange/nx
    } else {
      ny <- 50
      nx <- as.integer(ny * (xrange/yrange))
      cell.sz <- yrange/ny
    }  
    
    print(paste("local_identifier =", local_id))
    print("Home range dimensions (pixels/voxels)")
    print(paste("nx =", nx, "ny =", ny))
    print("Home range dimensions (meters)")
    print(paste("xrange =", xrange, "yrange =", yrange))
    print(paste("cell size =", cell.sz))
    
    # Create home range using mkde
    mv.dat <- initializeMovementData(t, x, y, sig2obs=sig2obs, t.max=tmax)
    mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
    dens.res <- initializeDensity(mkde.obj, mv.dat)
    mkde.obj <- dens.res$mkde.obj
    mv.dat <- dens.res$move.dat
    plots <- append(plots, list(mkde.obj))
  }
  return(plots)
}

