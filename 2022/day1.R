input <- paste(readLines("input.txt"), collapse="\n")
by_elf <- strsplit(input, "\n\n")[[1]]
elf_sums <- unlist(
  lapply(by_elf, function(x){sum(as.numeric(strsplit(x, "\n")[[1]]))})
)
which.max(elf_sums)
max(elf_sums)

# Part 2
sum(tail(sort(elf_sums),3))
