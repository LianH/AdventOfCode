# Day 6

input <- read.csv("input.txt", stringsAsFactors=FALSE, header=FALSE, blank.lines.skip = FALSE)$V1
head(input)

yesses <- ""
groupcount <- list()
peoplecount <- 0
#input <- tail(input)
for(i in 1:length(input)){
  line <- input[i]
#  print(line)
  if((line=="") | (i==length(input))){
#    print(paste0("blankline, add: ", length(yesses)))
    groupcount[length(groupcount)+1] <- length(yesses)
    yesses <- ""
    peoplecount <- 0
  } else{
  peoplecount <- peoplecount+1
  if(peoplecount==1){
    yesses <- strsplit(line, "")[[1]]
  } else{
    # Part 1:
#    yesses <- c(yesses[!yesses %in% strsplit(line, "")[[1]]], strsplit(line, "")[[1]])
    # Part 2:
    yesses <- c(yesses[yesses %in% strsplit(line, "")[[1]]])
  }
  }
}
sum(unlist(groupcount))
