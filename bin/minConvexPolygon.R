minConvexPolygon <- function(gpsdata, utm.zone, datum, buffer, ids, include_mcp) {

   # Input
   # gpsdata:     data frame containing animal GPS data
   # utm.zone:    zone for UTM coordinates (with or without N/S designation)
   # datum:       data projection (e.g. WGS84 or NAD83)
   # buffer:      buffer space around the plot
   # ids:         identifiers of animals to be plotted
   # include_mcp: if true, show min convex polygon; otherwise only show data on map
   #
   # Output
   # mymap:       animal data, with or without min convex polygon, superimposed on map
   #
   # Usage example
   # minConvexPolygon(gpsdata, 11, "WGS84", c(269, 284), TRUE)
   # minConvexPolygon(gpsdata, 12, "NAD83, c(123, 234, 456), FALSE)
   #
   # Note - to get MCP/map to display in shiny app, call function without saving results to variable

   library(sp)
   library(adehabitatHR)
   library(scales)
   library(ggmap) 

   # convert animal id to string as needed by geom_polygon
   gpsdata$id <- as.character(gpsdata$id)

   # Generate the basemap using all data
   gpsdata.sp <- gpsdata[, c("id", "xdata", "ydata")]
   coordinates(gpsdata.sp) <- c("xdata", "ydata")
   crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
   proj4string(gpsdata.sp) <- CRS(crsstr)
   gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
   mybasemap <- get_stamenmap(bbox = c(left = min(gpsdata.spgeo@coords[,1])-buffer, 
                                    bottom = min(gpsdata.spgeo@coords[,2])-buffer, 
                                    right = max(gpsdata.spgeo@coords[,1])+buffer, 
                                    top = max(gpsdata.spgeo@coords[,2])+buffer), 
                                    zoom = 8)

   # Filter data based on selected animal IDs
   ids <- as.character(ids)
   gpsdata.sp <- gpsdata.sp[gpsdata.sp$id %in% ids, ]

   # Prepare data and minimum convex polygon
   gpsdata.mcp <- mcp(gpsdata.sp, percent = 100)
   gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
   gpsdata.mcpgeo <- spTransform(gpsdata.mcp, CRS("+proj=longlat"))
   gpsdata.geo <- data.frame(gpsdata.spgeo@coords, id = gpsdata.spgeo@data$id)

   # Plot data points on basemap
   mymap <- ggmap(mybasemap) +
  	       geom_point(data = gpsdata.geo, 
	       aes(x = xdata, y = ydata, colour = id), size = 0.8, alpha = 0.5)  +
	       theme(legend.position = c(-0.2, 0.90)) +
	       labs(x = "Longitude", y = "Latitude")

   # Add minimum convex polygon
   if (include_mcp) {
      mymap <- mymap +
               geom_polygon(data = fortify(gpsdata.mcpgeo),
	       aes(long, lat, colour = id, fill = id),
	       alpha = 0.3) # alpha sets the transparency
   }

   return(mymap)
}
