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
# loaded. The animal_attribute() function is first defined and call to
# function is made at bottom of file

# --------------------------------------------------------------------

animal_attributes <- function(data_df) {

  # Replicate C printf functionality
  printf <- function(...) cat(sprintf(...))

  # Custom rounding to 2 significant digits if x > 10, 1 digit otherwise
  custom_round <- function(x) {return (signif(x, min(2,floor(log10(x)))))}

  animals <- append(as.list(sort(unique(data_df$local_identifier))), "all")
  max_pixels <- c(50, 100, 200, 500)

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
    }
    printf("\n")
  }
}

# Calling the function; assumes data_df is already defined
animal_attributes(data_df)

