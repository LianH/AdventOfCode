# Day 2
input <- as.character(read.csv("input.txt", header=F)$V1)

# Puzzle 1
head(input)
input_df1 <- t(data.frame(strsplit(input, ": ")))
input_df2 <- t(data.frame(strsplit(input_df1[,1], " ")))
input_df3 <- t(data.frame(strsplit(input_df2[,1], "-")))
input_df <- cbind(input_df1, input_df2[,2],input_df3)
counts <- apply(
  input_df, 1, function(x){sum(gregexpr(x[3],x[2])[[1]]>0)}
)
output_df <- cbind(cbind(input_df, counts), data.frame((counts >= as.numeric(input_df[,4])) & (counts <= as.numeric(input_df[,5]))))
sum(
  output_df[,7]
    )
head(output_df)

# Puzzle 2
head(input_df)
firstonepresent <- apply(
  input_df, 1, function(x){x[4] %in% gregexpr(x[3],x[2])[[1]]}
)
secondonepresent <- apply(
  input_df, 1, function(x){x[5] %in% gregexpr(x[3],x[2])[[1]]}
)
sum((firstonepresent+secondonepresent)==1)
