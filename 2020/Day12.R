# Day 12

input <- (read.table("input.txt", header=F, blank.lines.skip = FALSE, sep="", comment.char="")$V1)

# Part 1
posn <- c(0, 0)
direction <- 90
for(order in input){
  letter <- substring(order, 1, 1)
  amount <- as.numeric(substring(order, 2, nchar(order)))
  if(letter=="R"){
    direction <- (direction + amount)%%360
  } else if(letter=="L"){
    direction <- (direction - amount)%%360
  } else if(letter=="F"){
    posn[1] <- posn[1] + amount*(direction==90) - amount*(direction==270)
    posn[2] <- posn[2] + amount*(direction==0) - amount*(direction==180)
  } else{
    posn[1] <- posn[1] + amount*(letter=="E") - amount*(letter=="W")
    posn[2] <- posn[2] + amount*(letter=="N") - amount*(letter=="S")
  }
}
sum(abs(posn))

# Part 2

input <- c("F10",
           "N3",
           "F7",
           "R90",
           "F11")

ship_posn <- c(0, 0)
waypoint_posn <- c(10, 1)
for(order in input){
  letter <- substring(order, 1, 1)
  amount <- as.numeric(substring(order, 2, nchar(order)))
  if(letter%in%c("R","L")){
    clockwise <- ((360-amount)*(letter=="L")+amount*(letter=="R"))/90
    if(clockwise==1){
      waypoint_posn <- c(waypoint_posn[2], waypoint_posn[1]*-1)
    } else if(clockwise==2){
      waypoint_posn <- c(waypoint_posn[1]*-1, waypoint_posn[2]*-1)
    } else if(clockwise==3){
      waypoint_posn <- c(waypoint_posn[2]*-1, waypoint_posn[1])
    } else{
      print("SOMETHING HAS GONE TERRIBLY WRONG")
    }
  } else if(letter=="F"){
    ship_posn <- ship_posn+waypoint_posn*amount
  } else{
    waypoint_posn[1] <- waypoint_posn[1] + amount*(letter=="E") - amount*(letter=="W")
    waypoint_posn[2] <- waypoint_posn[2] + amount*(letter=="N") - amount*(letter=="S")
  }
#  print(order)
#  print(ship_posn)
#  print(waypoint_posn)
}
sum(abs(ship_posn))
