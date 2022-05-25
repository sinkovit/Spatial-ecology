# The following code snippet shows how to list the stats for each
# animal and for the data set as a whole. It also provides advice on
# choosing pixel size.

# data_df is the data frame containg data that has already been
# transformed so that location_[lat|long].1 contain the azimuthal
# equidistance projection and t is time in minutes relative to the
# first observation in the entire data set

# Note that I'm using the R range() function for both performance and
# to maintain more compact code. It returns a vector of min and max
# values

# --------------------------------------------------------------------

animal_attributes <- function(data_df) {

  printf <- function(...) cat(sprintf(...))

  animals <- append(as.list(sort(unique(data_df$local_identifier))), "all")
  max_pixels <- c(50, 100, 200, 500)

  printf("%7s ", "id")
  printf("%9s %9s %9s %9s %10s %10s ", "long_min", "long_max", "lat_min", "lat_max", "x_range", "y_range")
  for (max_pix in max_pixels) {
    printf("%7s %7s ", "dims", "size")
  }
  printf("\n------- --------- --------- --------- --------- ---------- ---------- ------- ------- ")
  printf("------- ------- ------- ------- ------- -------\n")

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
    printf("%9.4f %9.4f %9.4f %9.4f %10.2f %10.2f ", long_minmax[1], long_minmax[2], lat_minmax[1], lat_minmax[2], x_range, y_range)

    for (max_pix in max_pixels) {
      if (x_range >= y_range) {
         nx <- max_pix
         ny <- as.integer(nx * (y_range/x_range))
         cell.sz <- x_range/nx
      } else {
         ny <- max_pix
         nx <- as.integer(ny * (x_range/y_range))
         cell.sz <- y_range/ny
      }
      dims <- sprintf("%dx%d", nx, ny)
      printf("%7s %7.2f ", dims, cell.sz)
    }
    printf("\n")
  }
}

animal_attributes(data_df)

