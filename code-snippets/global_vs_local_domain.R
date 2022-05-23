# The following code snippet shows how to choose between plotting all
# animal home ranges on the same spatial domain, determined by the
# min/max latitude/logitude across the entire data set, or if each
# animal should be plotted on its own home range, determined by that
# animal's own min/max latitude/longitude.

# This block of code can be called inside the loop over animals,
# although in the long run we might want to move outside the loop

# data_df is the data frame containg data that has already been
# transformed so that location_[lat|long].1 contain the azimuthal
# equidistance projection

# x and y are slices of the data frame specific to the animal
# x <- data_df[which(data_df$local_identifier == local_id), "location_long.1"]
# y <- data_df[which(data_df$local_identifier == local_id), "location_lat.1"]

if(global_domain) {
  # All home ranges on the same domain
  xmin <- min(data_df$location_long.1)
  ymin <- min(data_df$location_lat.1)
  xmax <- max(data_df$location_long.1)
  ymax <- max(data_df$location_lat.1)
} else {
  # Each animal plotted on own domain
  xmin <- min(x)
  ymin <- min(y)
  xmax <- max(x)
  ymax <- max(y)
}
