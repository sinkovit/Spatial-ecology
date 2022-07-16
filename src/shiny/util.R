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


# From Bob's code
# @ https://github.com/sinkovit/Spatial-ecology/blob/Bob/code-snippets/animal_stats.R
# The following code snippet shows how to list the stats for each
# animal and for the data set as a whole. It also provides advice on
# choosing pixel size.

# data_df is the data frame containg data that has already been
# transformed so that location_[lat|long].1 contain the azimuthal
# equidistance projection and t is time in minutes relative to the
# first observation in the entire data set

# Note 1 - I'm using the R range() function for both performance and
# to maintain more compact code. It returns a vector of min and max
# values

# Note 2 - code is written assuming that data has already been
# loaded. The animalAttributes() function is first defined and call to
# function is made at bottom of file

# --------------------------------------------------------------------

animalAttributes <- function(data_df) {
  printf("Calculating spatial attributes...\n")

  # Replicate C printf functionality
  printf <- function(...) cat(sprintf(...))
  
  # Custom rounding to 2 significant digits if x > 10, 1 digit otherwise
  custom_round <- function(x) {return (signif(x, min(2,floor(log10(x)))))}
  
  animals <- append(as.list(sort(unique(data_df$local_identifier))), "all")
  #print(paste("animals =", animals))
  max_pixels <- c(30, 60, 100, 300)
  #print(paste("max_pixels =", max_pixels))
  
  printf("\n")
  printf("The following table gives the extent of animal movement for the\n")
  printf("individuals and for the data set as a whole, along with the grid\n")
  printf("dimensions resulting from various pixel sizes. Keep in mind that larger\n")
  printf("grids result in longer calculations, so you may want to choose a pixel\n")
  printf("size that result in a smaller grid for preliminary calculations.\n")
  printf("\n")
  printf("E-W(m) and N-S(m) are the east-west and north-south ranges, in meters\n")
  printf("px(m) is the pixel size in meters and grid is the resulting grid dimensions\n")
  printf("\n")
  
  printf("%7s ", "id")
  printf("%9s %9s %9s %9s %10s %10s ", "long_min", "long_max", "lat_min", "lat_max", "E-W(m)  ", "N-S(m)  ")
  for (max_pix in max_pixels) {
    printf("%6s %7s ", "px(m)", "grid")
  }
  printf("\n------- --------- --------- --------- --------- ---------- ---------- ------ ------- ")
  printf("------ ------- ------ ------- ------ -------\n")
  
  #result <- data.frame(id = numeric(), 'long min' = numeric(),
  #                     long_max = numeric(), lat_min = numeric(),
  #                     lat_max = numeric(), E_W = numeric(),
  #                     N_S = numeric())
  #, px(m) = numeric(), grid = character(),
  #                     px(m) = numeric(), grid = character(), px(m) = numeric(),
  #                     grid = character(), px(m) = numeric(), grid = character() )
  # Oddly above doesn't work but below works...
  result <- data.frame(id = numeric())
  result[ , 'longitude (min)'] <- numeric()
  result[ , 'longitude (max)'] <- numeric()
  result[ , 'latitude (min)'] <- numeric()
  result[ , 'latitude (max)'] <- numeric()
  result[ , 'East-West (m)'] <- numeric()
  result[ , 'North-South (m)'] <- numeric()
  row.index <- 1

  for (local_id in animals) {
    if(local_id == "all") {
      long_minmax = range(data_df$location_long)
      lat_minmax = range(data_df$location_lat)
      x_minmax = range(data_df$location_long.1)
      y_minmax = range(data_df$location_lat.1)
      t_minmax = range(data_df$time)
      printf("    all ")
    } else {
      long_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_long"])
      lat_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_lat"])
      x_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_long.1"])
      y_minmax = range(data_df[which(data_df$local_identifier == local_id), "location_lat.1"])
      t_minmax = range(data_df[which(data_df$local_identifier == local_id), "time"])
      printf("%7i ", local_id)
    }
    x_range <- x_minmax[2] - x_minmax[1]
    y_range <- y_minmax[2] - y_minmax[1]
    printf("%9.4f %9.4f %9.4f %9.4f %10.2f %10.2f ", long_minmax[1],
           long_minmax[2], lat_minmax[1], lat_minmax[2], x_range, y_range)
    row <- c(local_id, round(long_minmax[1],3), round(long_minmax[2],3),
             round(lat_minmax[1],3), round(lat_minmax[2],3), round(x_range,2),
             round(y_range,2))

    row.tail = c()
    for (max_pix in max_pixels) {
      if (x_range >= y_range) {
        nx <- max_pix
        ny <- as.integer(nx * (y_range/x_range))
        cell.sz <- x_range/nx
        cell.sz <- custom_round(cell.sz)
        nx <- as.integer(x_range/cell.sz)
        ny <- as.integer(y_range/cell.sz)
      } else {
        ny <- max_pix
        nx <- as.integer(ny * (x_range/y_range))
        cell.sz <- y_range/ny
        cell.sz <- custom_round(cell.sz)
        nx <- as.integer(x_range/cell.sz)
        ny <- as.integer(y_range/cell.sz)
      }
      dims <- sprintf("%dx%d", nx, ny)
      printf("%6.1f %7s ", cell.sz, dims)

      if(row.index == 1) {
        cell.size.label <- paste('cell size (',max_pix, "m)", sep = "")
        dim.label <- paste('grid dimensions (', max_pix, 'm)', sep = "")
        #cell.size.label <- "cell size (m)"
        columns.new <- c(cell.size.label, dim.label)
        result[ , cell.size.label] <- numeric()
        result[ , dim.label] <- character()
      }

      row.tail <- append(row.tail, c(cell.sz, dims))
    }
    row <- append(row,row.tail)
    result[row.index, ] <- row
    row.index <- row.index + 1
    
    printf("\n")
  }
  return(result)
}


