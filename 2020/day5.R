# Day 5

input <- read.csv("input.txt", stringsAsFactors=FALSE, header=FALSE)$V1

# Part 1
get_seat_id <- function(x, totalrows = 128, totalseats = 8){
  added <- 0
  for(i in 1:7){
    letter <- substring(x, i, i)
    add <- (letter == "B") * totalrows/2^i
    added <- added+add
  #  print(paste(letter, "means we add", add, "and thus now start at", added))
  }
  added_seat <- 0
  for(i in 8:10){
    letter <- substring(x, i, i)
    add_seat <- (letter=="R") * totalseats/2^(i-7)
    added_seat <- add_seat + added_seat
  #  print(paste(letter, "means we add", add_seat, "and thus now start at", added_seat))
  }
  return(added * 8 + added_seat)
}
seat_ids <- unlist(lapply(input, get_seat_id))
max(seat_ids)

# Part 2
sort(seat_ids)
(min(seat_ids):max(seat_ids))[!min(seat_ids):max(seat_ids) %in% seat_ids]