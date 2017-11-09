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



