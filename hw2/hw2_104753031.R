# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_104753031.R --target male/female --files meth1 meth2 … methx --out result.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    target<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag: ", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("target     :", target))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
ms<-c()
sen<-c()
sep<-c()
f1<-c()
auc<-c()
for(file in files)
{
  # read data
  d<-read.table(file, header=T,sep=",")
  
  # record method name
  name<-gsub(".csv", "", basename(file))
  ms<-c(ms,name)
  
  # compute confusion table,TP,TN,FP,FN
  cM <- table(truth=d$reference, prediction=d$prediction)
  if(target == "male"){
    TP<-cM[["male","male"]]
    TN<-cM[["female","female"]]
    FP<-cM[["female","male"]]
    FN<-cM[["male","female"]]
  }else if(target == "female"){
    TP<-cM[["female","female"]]
    TN<-cM[["male","male"]]
    FP<-cM[["male","female"]]
    FN<-cM[["female","male"]]
  }else{
    stop(paste("Unknown target: ", target), call.=FALSE)
  }
  
  # compute sensitivity, specificity, F1, AUC
  sen<-c(sen,TP/(TP+FN))
  sep<-c(sep,TN/(TN+FP))
  f1<-c(f1,2*TP/(2*TP+FP+FN))
  
  # compute AUC
  library('ROCR')
  eval <- prediction(d$pred.score,d$reference)
  value<-attributes(performance(eval,'auc'))$y.values[[1]]
  auc<-c(auc,value)
}
out_data<-data.frame(method=ms, sensitivity=sen, specificity=sep, F1=f1, AUC=auc, stringsAsFactors = F)
index<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], which.max)

# output file
out_data<-rbind(out_data,c(highest,ms[index]))
write.table(out_data, file=out_f, row.ms = F, quote = F)
