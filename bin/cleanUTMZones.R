cleanUTMZones <- function(zones) {

   # Takes output from preprocessDataFrame(), removes North/South
   # identifiers, coverts to integer and returns list

   clean.zones <- list()
   for (z in unlist(zones)) {
       z <- gsub("N", "", z)
       z <- gsub("S", "", z)
       z <- gsub("n", "", z)
       z <- gsub("s", "", z)
       z <- as.integer(z)
       clean.zones <- append(clean.zones, z)
   }
   return(clean.zones)
}
