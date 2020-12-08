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

split_line <- function(line, loc){
  start <- gsub(", ", "", gsub("bags","bag", sub("[.]", "", substring(line, loc+13, nchar(line)))))
  blub <- strsplit(start, " bag")
  lapply(blub, function(x){strsplit(sub(" ", "|", x), "[|]")})
}
final_counts <- head(data.frame("color"=NA, "finalcount"=0),0)
loop <- 1
while(!"shiny gold" %in% final_counts$color){
  print(loop)
  loop <- loop+1
for(line in input){
  loc <- regexpr("bags contain",line)
  color <- substring(line, 1, loc-2)
  if(grepl("no other bags", line)){
    final_counts <- rbind(final_counts, data.frame("color"=color, "finalcount"=1))
  } else{
    contained_colors <- split_line(line, loc)[[1]]
    for(i in 1:length(contained_colors)){
      precalc_count <- final_counts[final_counts$color==contained_colors[[i]][2],"finalcount"]
      contained_colors[[i]][3] <- ifelse(length(precalc_count)==0, NA, precalc_count)
    }
    multipliers <- as.numeric(unlist(lapply(contained_colors, function(x){x[1]})))
    bagcounts <- as.numeric(unlist(lapply(contained_colors, function(x){x[3]})))
    if(all(!is.na(bagcounts))){
      total_count <- sum(multipliers*bagcounts) + 1
      final_counts <- rbind(final_counts, data.frame("color"=color, "finalcount"=total_count))
      if(color=="shiny gold"){
        break
      }
    }
  }
}
}
final_counts[final_counts$color=="shiny gold","finalcount"]-1 # Subtract one because for shiny gold we don't want to count itself
