input <- read.csv("input.txt", header=F)
find_duplicate <- function(x){
  nritems <- nchar(x)
  items <- strsplit(x, "")[[1]]
  items_comp1 <- items[1:(nritems/2)]
  items_comp2 <- items[(nritems/2+1):nritems]
  dup <- items_comp1[which(items_comp1 %in% items_comp2)]
  return(unique(dup))
}
input$dup <- unlist(lapply(input$V1, find_duplicate))
input$dup

myLetters <- c(
  letters[1:26],
  toupper(letters[1:26])
  )

sum(match(input$dup, myLetters))

#############################

input$grpnr <- rep(1:(nrow(input)/3), each=3)
badge <- list(NULL)
for(group in 1:max(input$grpnr)){
  one <- strsplit(input$V1[(group-1)*3+1],"")[[1]]
  two <- strsplit(input$V1[(group-1)*3+2],"")[[1]]
  three <- strsplit(input$V1[(group-1)*3+3],"")[[1]]
  badge <- c(badge, intersect(intersect(one, two), three))
}
sum(match(unlist(badge), myLetters))
