# Day 11

input <- (read.table("input.txt", header=F, blank.lines.skip = FALSE, sep="", comment.char="")$V1)

input <- c("L.LL.LL.LL",
           "LLLLLLL.LL",
"L.L.L..L..",
"LLLL.LL.LL",
"L.LL.LL.LL",
"L.LLLLL.LL",
"..L.L.....",
"LLLLLLLLLL",
"L.LLLLLL.L",
"L.LLLLL.LL")
mat <- matrix(unlist(lapply(input, function(x){strsplit(x, "")})), nrow=length(input), byrow=T)
mat <- cbind(rep(".", nrow(mat)), mat, rep(".", nrow(mat)))
mat <- rbind(rep(".", ncol(mat)), mat, rep(".", ncol(mat)))
new_mat <- mat
counter <- 1

# Part 2
while((mean(new_mat==mat)<1) | (counter==1)){
  print(paste("counter: ", counter))
  print(new_mat)
  counter <- counter + 1
  mat <- new_mat
  for(i in 2:(nrow(mat)-1)){
    for(j in 2:(ncol(mat)-1)){
      step <- 1
      while((mat[i-step, j-step]==".") & ((i-step-1)>=1) & ((j-step-1)>=1)){
        step <- step+1
      }
      adjacent1 <- mat[i-step, j-step]
      step <- 1
      while((mat[i-step, j]==".") & ((i-step-1)>=1)){
        step <- step+1
      }
      adjacent2 <- mat[i-step, j]
      step <- 1
      while((mat[i-step, j+step]==".") & ((i-step-1)>=1) & ((j+step+1)<=ncol(mat))){
        step <- step+1
      }
      adjacent3 <- mat[i-step, j+step]
      step <- 1
      while((mat[i, j-step]==".") & ((j-step-1)>=1)){
        step <- step+1
      }
      adjacent4 <- mat[i, j-step]
      step <- 1
      while((mat[i, j+step]==".") & ((j+step+1)<=ncol(mat))){
        step <- step+1
      }
      adjacent5 <- mat[i, j+step]
      step <- 1
      while((mat[i+step, j-step]==".") & ((i+step+1)<=nrow(mat)) & ((j-step-1)>=1)){
        step <- step+1
      }
      adjacent6 <- mat[i+step, j-step]
      step <- 1
      while((mat[i+step, j]==".") & ((i+step+1)<=nrow(mat))){
        step <- step+1
      }
      adjacent7 <- mat[i+step, j]
      step <- 1
      while((mat[i+step, j+step]==".") &  ((i+step+1)<=nrow(mat)) & ((j+step+1)<=ncol(mat))){
        step <- step+1
      }
      adjacent8 <- mat[i+step, j+step]
      adjacents <- c(adjacent1, adjacent2, adjacent3, 
                     adjacent4, adjacent5, 
                     adjacent6, adjacent7, adjacent8)
      if(mat[i, j]=="L"){
        if(mean(adjacents=="#")==0){
          new_mat[i,j] = "#"
        }
      } else if(mat[i,j]=="#"){
          if(sum(adjacents=="#")>=5){
            new_mat[i,j] = "L"
          }
        }
    }
  }
}

sum(mat=="#")
mean((mat==".") == (new_mat=="."))
