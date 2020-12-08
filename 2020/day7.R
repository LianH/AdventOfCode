input <- read.csv("input.txt", stringsAsFactors=FALSE, header=FALSE, blank.lines.skip = FALSE, sep="|")$V1

input <- c("light red bags contain 1 bright white bag, 2 muted yellow bags.",
           "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
           "bright white bags contain 1 shiny gold bag.",
           "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
           "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
           "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
           "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
           "faded blue bags contain no other bags.",
           "dotted black bags contain no other bags.")
# Part 1
what_contains_y <- function(line, y){
  loc <- regexpr("bags contain",line)
  if(grepl(y, substring(line, loc, nchar(line)))){
    return(substring(line, 1, loc-2))
  } else
    return(NULL)
}
outerbags <- c("shiny gold")
count_shiny_gold <- FALSE
beginning_outerbags <- c(NULL)
loop <- 0
while(length(beginning_outerbags) < length(outerbags)){
  loop <- loop + 1
  print(paste0(loop,": previous started with ", length(beginning_outerbags), " colors, ended with ", length(outerbags)))
  beginning_outerbags <- outerbags
  for(line in input){
    for(innerbag in outerbags){
      new_outer <- what_contains_y(line, innerbag)
      if(length(new_outer)>0){
        if(new_outer=="shiny gold"){
          count_shiny_gold <- TRUE
        }
      }
      outerbags <- unique(c(outerbags, new_outer))
    }
  }
}
length(outerbags)-(!count_shiny_gold)

# Part 2
