input <- read.csv("input.txt", header=F)
input$elf1_start <- as.numeric(gsub("-.*","",input$V1))
input$elf1_end <- as.numeric(gsub(".*-","",input$V1))
input$elf2_start <- as.numeric(gsub("-.*","",input$V2))
input$elf2_end <- as.numeric(gsub(".*-","",input$V2))
input$one_in_two <- (input$elf1_start >= input$elf2_start) &
  (input$elf1_end <= input$elf2_end)
input$two_in_one <- (input$elf2_start >= input$elf1_start) &
  (input$elf2_end <= input$elf1_end)
sum(input$one_in_two | input$two_in_one)

# Part 2

# Find all the ones with no overlap, take the inverse 
sum(!(((input$elf1_start < input$elf2_start) &
  (input$elf1_end < input$elf2_start)) |
((input$elf2_start < input$elf1_start) &
     (input$elf2_end < input$elf1_start))))



