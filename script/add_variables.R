# functions

get_distance <- function(a){
  pts <- a[c("Longitude","Latitude")]
  distance <- distHaversine(p1= pts[-nrow(a),],
                          p2=pts[-1,])
  distance <- append(distance, 0, 0)
  return (distance)
}

get_bearing <- function(a){
  pts <- a[c("Longitude","Latitude")]
  bearing <- bearing(p1=pts[-nrow(a),],
                     p2=pts[-1,])
  bearing <- append(bearing, 0,0)
  return (bearing)
}

#add labels
add_labels<-function(data,label){
  label <-label$flag_mode_imu[match(data$Mag_Z.mgauss.,label$Mag_Z.mgauss.)]
  return (label)
}

# add polyLines

points_to_line <- function(data, long, lat, id_field = NULL) {
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

