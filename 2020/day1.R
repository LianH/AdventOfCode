# Day 1
input <- read.csv("input.txt", header=F)$V1

# Puzzle 1
indices <- which((2020-input) %in% input)
input[indices[1]]*input[indices[2]]

# Puzzle 2
sum1mat <- matrix(rep(input, each=length(input)), nrow=length(input), byrow=F) + 
  matrix(rep(input, each=length(input)), nrow=length(input), byrow=T)
indices2 <- which(input %in% (2020-sum1mat))
input[indices2[1]]*input[indices2[2]]*input[indices2[3]]
