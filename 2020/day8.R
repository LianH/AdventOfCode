input <- read.csv("input.txt", stringsAsFactors=FALSE, header=FALSE, blank.lines.skip = FALSE, sep="|")$V1

# Part 2 (adapted from part 1)
changed_positions <- c()
resultfound <- FALSE
while(!resultfound){
  changed <- FALSE
  position <- 1
  accumulator <- 0
  positions_seen <- c()
  while((!position %in% positions_seen)){
    positions_seen <- c(positions_seen, position)
    action <- substring(input[position], 1, 3)
    howmuch <- as.numeric(substring(input[position], 5, nchar(input[position])))
    if(action=="nop"){
      if(changed | position %in% changed_positions){
        position <- position + 1
      } else{
        changed_positions <- c(changed_positions, position)
        changed <- TRUE
        position <- position + howmuch
      }
    } else if(action=="acc"){
      accumulator <- accumulator + howmuch
      position <- position + 1
    } else if(action=="jmp"){
      if(changed | position %in% changed_positions){
        position <- position + howmuch
      } else{
        changed_positions <- c(changed_positions, position)
        changed <- TRUE
        position <- position + 1
      }
    }
    if(position>length(input)){
      resultfound <- TRUE
      break
    }
  }
}