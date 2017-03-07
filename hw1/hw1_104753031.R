##parse input argument

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R -files test.1.csv -out result.csv", call.=FALSE)
} else if (length(args)==1) {
  i_f <- args[1] 
  print(i_f)
  
}

i <- 1
i_f <- c()
while(i < length(args)) {
  if ( args[i] == "-files") {
    i <- i+1
    j <- 1
    while(!startsWith(args[i], "-")&&(i <= length(args))) {
      i_f[j] <- args[i]
      i <- i+1
      j <- j+1
    }
    
    
  }
  if (args[i] == "-out") {
    o_f <- args[i+1]
  }
  i <- i+1
}


## write output data

set <- NULL
weight <- NULL
height <- NULL

for (i in c(1:length(i_f))) {
  f <- i_f[i]
  set_name <- substr(f, 1, nchar(f)-4)
  set <- c(set,set_name)
  data <- read.csv(f)
  weight <- c(weight,round(max(data["weight"]),digit=2))
  height <- c(height,round(max(data["height"]),digit=2))
  
}

out <- data.frame(set, weight, height)


write.csv(out, file=o_f, row.names=FALSE)
