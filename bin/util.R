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

# function - determine if the given haystack contains the needle
# haystack - can be a single variable or a vector
# needle - value to look for in the haystack
# returns - TRUE if the needle is in the haystack; FALSE otherwise
contains <- function(haystack, needle) {
  if (isEmpty(haystack)) {
    if (isEmpty(needle)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  if (is.vector(haystack)) {
    for (i in 1:length(haystack)) {
      if (haystack[i] == needle) {
        return(TRUE)
      }
    }
    return(FALSE)
  } else if (haystack == needle) {
    return(TRUE)
  }

  return(FALSE)
}


# Get all the projects from the given root and append "/files" since that's how
# Hubzero store the files for each project
GetProjects <- function(root, session) {
  if (dir.exists(root)) {
    dirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
    projects = c()
    for (i in 1:length(dirs)) {
      volume_name <- dirs[i]
      files_path <- paste(root, "/", dirs[i], "/files", sep = "")
      if (dir.exists(files_path)) {
        projects[volume_name] = files_path
        # print(projects)
      }
    }
    return (projects)
  } else
    return(NULL)
}

# Returns TRUE of the parameter is NULL, a string with no non-space characters,
# or length < 1; otherwise returns FALSE
isEmpty <- function(x) {
  # print(paste("isEmpty x =", x))
  # print(paste("class =", class(x)))
  # print(paste("str =", str(x)))
  # print(paste("length =", length(x)))
  
  if (is.null(x)) {
    return(TRUE)
  }
  
  if (length(x) < 1) {
    return(TRUE)
  }
  
  if (is.vector(x)) {
    for (i in 1:length(x)) {
      if (is.character(x[i]) && nchar(str_replace_all(x[i], " ", "")) < 1) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  if (is.character(x) && nchar(str_replace_all(x, " ", "")) < 1) {
    return(TRUE)
  }
  
  return(FALSE)
}

save_output <- function(types, rasters, id, utm.zone, datum, basename) {
  if (length(rasters) == 0) {
    return(NULL)
  }
  
  if (is.null(basename)) {
    basename <- "noname"
  }
  
  for (raster in rasters) {
    if (is.null(id) || raster$id == id) {
      contour_info <- raster$contours
      crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum,
                      " +units=m +no_defs", sep="")
      output_file <- file.path(path_home(), paste0(basename, "-", raster$id))
      
      if ("raster" %in% types) {
        terra::writeRaster(contour_info$raster, filename = paste0(output_file, ".asc"), overwrite = TRUE)
      }
      
      if ("shape" %in% types) {
        contour_lines <- terraToContour(contour_info$raster, contour_info$contour$threshold, crsstr = paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep=""))
        sf::st_write(contour_lines, paste0(output_file, "_Contours.shp"), overwrite=TRUE)
      }
    }
  }
}

# Read the Stadia map API key file, remove newline if any is present, and setup
# session with the key. Unfortunately there is no way to test if the api key is
# valid or not.  If the key is not valid, the error "Error in
# curl::curl_fetch_memory(url, handle = handle): URL rejected: Malformed input
# to a URL function" will be caught at plot run-time
setupAPIkey <- function() {
  filename <- "/secrets/mkde.txt"
  key <- read_file(filename)
  key <- gsub("[\r\n]", "", key)
  register_stadiamaps(key = key, write = FALSE)
  return(TRUE)
}
