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


# Load data from Movebank
# Returns a list where first item is the data and second item is the error
# message; if successful, data will be populated and there may be a message to
# display to the user; if failure, data will be NULL and the message will contain
# the error message
loadDataframeFromMB <- function(study, username, password, remove_outliers) {
  # print(paste("username =", username))
  # print(paste("password =", password))
  # print(paste("study = ", study))
  # print(paste("remove_outliers = ", remove_outliers))
  # file.local <- paste(getwd(), "/Study-", toString(study), ".RData", sep="")
  #print(paste("file.local =", file.local))
  
  # move2
  key <- keyring::key_list(service="movebank")
  # print(paste("move2 key =", key, "\n"))
  if (nrow(key) == 0) {
    # print("move2 no key!\n")
    move2::movebank_store_credentials(username, password)
    # print("move2 stored mb credential\n")
  # } else {
  #   print("move2 key found!\n")
  }
  
  tryCatch({
    # # if(file.exists(file.local)) {
    # #   printf("  data exists locally, loading...")
    # #   load(file.local)
    # #   printf("done\n")
    # #   return(list(data, NULL))
    # # } else {
    
    # # move (1) - this can be used for debugging if move2 is throwing an error
    # # on a study.  Be sure to uncomment the library(move) statement in app.R
    # # so that the original move package will be available
    # login <- move::movebankLogin(username = username, password = password )
    # # # maybe try shiny::invalidateLater()?
    # # # save(data, file=file.local)
    # # data <- getMovebankData(study = strtoi(study), login = login)
    # data1 <- move::getMovebankLocationData(study = strtoi(study), login = login)
    # # move (1) returns whatever attributes the study has, including the following
    # # algorithm.marked.outlier, deployment.id, external.temperature, event.id,
    # # gps.fix.type.raw, gps.hdop, gps.maximum.signal.strength, gps.satellite.count,
    # # gps.time.to.fix, gps.vdop, ground.speed, heading, height.above.ellipsoid,
    # # height.above.msl, height.raw, individual.id, individual.local.identifier,
    # # individual.taxon.canonical.name, location.error.text, location.lat,
    # # location.long, manually.marked.outlier, raptor.workshop.migration.state,
    # # sensor.type, sensor.type.id, study.id, study.name, tag.id,
    # # tag.local.identifier, tag.voltage, telemetry.run.id, timestamp,
    # # vertical.error.numerical, visible
    # print(paste("data1 read in class =", class(data1)))
    # print(paste("data1 read in length =", length(data1)))
    # print(paste("data1 read in nrow =", nrow(data1)))
    # # print(paste("data1 read in colnames =", colnames(data1)))
    # print(paste("data1 read in names =", names(data1)))
    # # print(paste("data head =", head(data)))
    # #print(paste("data summary:", summary(data)))
    # return(list(data1, NULL))
    
    # move2
    # note, movebank_download_study returns a move2 objects and contains geometry
    # data2 <- move2::movebank_download_study(
    #   study_id=strtoi(study),
    #   #attributes = "all",
    #   attributes = c("event_id", "ground_speed", "heading", "height_raw",
    #                  "individual_local_identifier",
    #                  "individual_taxon_canonical_name", "location_lat",
    #                  "location_long", "sensor_type_id", "timestamp", "visible"),
    #   remove_movebank_outliers=remove_outliers)
    
    # for debugging...download the complete study
    # tmp <- move2::movebank_download_study(study_id = strtoi(study),
    #                                 remove_movebank_outliers = remove_outliers)
    # print(paste("tmp class =", class(tmp)))
    # print(paste("tmp length =", length(tmp)))
    # print(paste("tmp nrow =", nrow(tmp)))
    # print(paste("tmp names =", names(tmp)))
    
    # # tried to get x and y from geometry...
    # geom <- data["geometry"]
    # print(paste("geom class =", class(geom)))
    # df <- sf::st_sf(geometry = geom)
    # print(paste("df class =", class(df)))
    # print(paste("df length =", length(df)))
    # print(paste("df nrow =", nrow(df)))
    # print(paste("df colnames =", colnames(df)))
    
    # # convert move2 to move...no column names!
    # results <- move2::to_move(data2)
    
    # # following is needed if using movebank_download_study
    # results <- dplyr::mutate(data2, long = sf::st_coordinates(data2)[,1],
    #                          lat = sf::st_coordinates(data2)[,2])

    # move(1) has additional attributes study.name and sensor.type but that
    # causes runtime error below...

    # movebank_retrieve(
    #   entity_type = "tag_type",
    #   attributes = c("external_id", "id")
    # )

    # sensors <- movebank_retrieve(entity_type = "tag_type", study_id = strtoi(study))
    # print(paste("sensors length =", length(sensors)))
    # print(paste("sensors nrow =", nrow(sensors)))
    # print(paste("sensors colnames =", colnames(sensors)))
    # print(paste("sensors head =", head(sensors)))
    # print(paste("sensors :", sensors))
    #
    # # movebank_retrieve returns a data frame/tibble (no)
    # data <- movebank_retrieve(entity_type = 'event', study_id = strtoi(study),
    #                           attributes = c("deployment_id", "event_id",
    #                                          "ground_speed", "heading",
    #                                          "height_raw", "individual_id",
    #                                          "individual_local_identifier",
    #                                          "individual_taxon_canonical_name",
    #                                          "location_lat", "location_long",
    #                                          "sensor_type_id", "study_id",
    #                                          "tag_id", "tag_local_identifier",
    #                                          "timestamp", "visible"),
    #                            remove_movebank_outliers=remove_outliers)
    
    # # for debugging...get the study's sensors
    # study_sensors <- movebank_download_study_info(study_id = study)$sensor_type_ids
    # print(paste("study_sensors =", study_sensors))

    # get the attributes of the study to make sure that it has our minimum
    # required info
    # browser()
    attributes <- move2::movebank_retrieve(entity_type = "study_attribute",
                                           study_id = strtoi(study),
                                           sensor_type_id = "gps")$short_name
    # # following from Bart (https://gitlab.com/bartk/move2/-/issues/85) threw a
    # # runtime error
    # study_attributes <- move2::movebank_retrieve(
    #   entity_type = "study_attribute", study_id = study, sensor_type_id = "gps",
    #   attributes = c("short_name"))
    print(paste("attributes class =", class(attributes)))
    print(paste("attributes =", attributes))
    print(paste("attributes nrow =", nrow(attributes)))
    print(paste("attributes length =", length(attributes)))
    print(paste("attributes names =", names(attributes)))
    if (!length(attributes)) {
      return(list(NULL, "No attributes found for data!"))
    }
    
    # for debugging
    info <- move2::movebank_download_study_info(study_id = strtoi(study))
    print(paste("info class =", class(info)))
    print(paste("info nrow =", nrow(info)))
    print(paste("info length =", length(info)))
    print(paste("info names =", names(info)))
    # print(paste("info colnames =", colnames(info)))
    info_df <- as.data.frame(info)
    # for (i in 1:ncol(info_df)) {
    #   print(paste(i, "=", info_df[,i]))
    # }
    for (i in colnames(info_df)) {
      if (! is.na(info_df[,i])) {
        print(paste(i, "=", info_df[,i]))
      }
    }
    
    require_attributes <- c("location_long", "location_lat", "timestamp",
                            "individual_local_identifier")
    
    data2 <- move2::movebank_retrieve(
      entity_type = 'event', study_id = study, attributes = require_attributes,
      remove_movebank_outliers=remove_outliers)
    # attributes = c("event_id", "ground_speed",
    #                "heading", "height_raw",
    #                "individual_local_identifier",
    #                "individual_taxon_canonical_name",
    #                "location_lat", "location_long",
    #                "sensor_type_id", "timestamp",
    #                "visible"),
    print(paste("data2 read in class =", class(data2)))
    print(paste("data2 read in length =", length(data2)))
    print(paste("data2 read in nrow =", nrow(data2)))
    # print(paste("data2 read in str =", str(data2)))
    print(paste("data2 read in names =", names(data2)))
    # print(paste("data2 head =", head(data)))
    # print(paste("data summary:", summary(data)))
    # print("post data")
    # return(list(data, "Your Movebank account info has been saved"))

    # converting tibble to data.frame
    results <- as.data.frame(data2)
    # print(paste("results class =", class(results)))
    # print(paste("results length =", length(results)))
    # print(paste("results nrow =", nrow(results)))
    # print(paste("results names =", names(results)))
    return(list(results, "Your Movebank account info has been saved"))
  },
  error = function(error_message) {
    # message(rlang::last_error()$error)
    # rlang::last_error()$url
    # print(paste("error message x =", error_message))
    # print(paste("error message length =", length(error_message)))
    # print(paste("error message 0 =", error_message[0]))
    # print(paste("error message 1 =", error_message[1]))
    # print(paste("error message 2 =", error_message[2]))
    
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
      # following doesn't work
    # } else if (str_detect(error_message[1],
    #                       "non-existing attributes are requested")) {
    #   return(list(NULL, paste("One or more of the required attributes may be missing from the study")))
    } else if (str_detect(error_message[1], "stack usage") &&
               str_detect(error_message[1], "too close to the limit")) {
      return(list(NULL, "Out of memory"))
    } else if (str_detect(error_message[1], "The status message was: 503")) {
      return(list(NULL, "Movebank server currently unavailable"))
    }
    return(list(NULL,
                paste("Unexpected Movebank system error detected! Please <a href='https://uccommunityhub.hubzero.org/support/ticket/new' target='support'>submit a support ticket</a> and include this entire error message:",
                      error_message)))
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

