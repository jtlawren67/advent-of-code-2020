d12 <- readLines('data/day12.txt')

x = 0
y = 0
degrees = 0

#Part 1
for(direction in d12){
  measure = substr(direction, 1, 1)
  units = as.numeric(substr(direction, 2, nchar(direction)))
  
  if(measure == 'N'){y = y + units}
  else if(measure == 'S') {y = y - units}
  else if(measure == 'E') {x = x + units}
  else if(measure == 'W') {x = x - units}
  else if(measure == 'L') {degrees = (degrees + units)%%360}
  else if(measure == 'R') {degrees = (degrees - units)%%360}
  else if(measure == 'F'){
    if(degrees == 0){x = x + units}
    else if (degrees == 90){y = y + units}
    else if (degrees == 180) {x = x - units}
    else if (degrees == 270) {y = y - units}
  }
}

abs(x)+abs(y)

#Part 2
ship_x = 0
ship_y = 0
waypoint_x = 10
waypoint_y = 1
i = 0
for(direction in d12){
  i = i + 1
  measure = substr(direction, 1, 1)
  units = as.numeric(substr(direction, 2, nchar(direction)))
  
  if(measure == 'N'){waypoint_y = waypoint_y + units}
  else if(measure == 'S') {waypoint_y = waypoint_y - units}
  else if(measure == 'E') {waypoint_x = waypoint_x + units}
  else if(measure == 'W') {waypoint_x = waypoint_x - units}
  else if(measure == 'L') {
    degrees = units %% 360
    if(degrees == 90){
      temp = waypoint_y
      waypoint_y = waypoint_x 
      waypoint_x = -1*temp
    }
    else if(degrees == 180){
        waypoint_x = -1*waypoint_x
        waypoint_y = -1*waypoint_y
    }
    else if(degrees == 270){
      temp = waypoint_y
      waypoint_y = -1*waypoint_x;
      waypoint_x = temp
    }
  }
  else if(measure == 'R') {
    degrees = units %% 360
    if(degrees == 90){
      temp = waypoint_y
      waypoint_y = -1*waypoint_x 
      waypoint_x = temp
    }
    else if(degrees == 180){
      waypoint_x = -1*waypoint_x
      waypoint_y = -1*waypoint_y
    }
    else if(degrees == 270){
      temp = waypoint_y
      waypoint_y = waypoint_x;
      waypoint_x = -1*temp
    }
    
  }
  else if(measure == 'F'){
    ship_x = ship_x + (waypoint_x*units)
    ship_y = ship_y + (waypoint_y*units)
  }
  
}

abs(ship_x)+abs(ship_y)

