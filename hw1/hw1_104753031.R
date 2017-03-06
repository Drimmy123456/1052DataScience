

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R -files test.1.csv -out result.csv", call.=FALSE)
} else if (length(args)==1) {
  i_f <- args[1] 
  print(i_f)
  
}

i <- 1
while(i < length(args)) {
  if (args[i]=="-files") {
    i_f <- args[i+1]
  }
  if (args[i]=="-out") {
    o_f <- args[i+1]
  }
  i <- i+1
}


print(i_f)
print(o_f)