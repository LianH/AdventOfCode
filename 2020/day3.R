input <- as.character(read.csv("input.txt", header=F)$V1)

# Puzzle 1 & 2
# Puzzle 1: 
steplist <- list(c(1,3))
# Puzzle 2:
steplist <- list(c(1,1), c(1,3), c(1,5), c(1,7), c(2,1))
resultlist <- list()
for(steps in steplist){
nrrows <- length(input)
nrcols <- nchar(input[1])
nrtrees <- 0
position <- c(1, 1)
while(position[1] < nrrows){
  position <- position + steps
  position[2] <- ifelse(position[2]>nrcols, position[2]-nrcols, position[2])
  position_type <- substring(input[position[1]], position[2], position[2])
  nrtrees <- nrtrees + (position_type=="#")
}
resultlist[[length(resultlist)+1]] <- nrtrees
}
cumprod(unlist(resultlist))
