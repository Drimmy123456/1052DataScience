# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_104753031.R --target male/female --files meth1 meth2 ... methx --out result.csv", call.=FALSE)
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
  
  # compute TP,TN,FP,FN
  if(target == "male"){
    not_target<-"female"
  }else if(target == "female"){
    not_target<-"male"
  }else{
    stop(paste("Unknown target: ", target), call.=FALSE)
  }
  
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  i<-1 
  while(i < length(d$reference)){
    if(d$reference[i]==target && d$prediction==target){
      TP<-TP+1
    }else if(d$reference[i]==not_target && d$prediction==not_target){
      TN<-TN+1
    }else if(d$reference[i]==not_target && d$prediction==target){
      FP<-FP+1
    }else if(d$reference[i]==target && d$prediction==not_target){
      FN<-FN+1
    }
    
    
    i<-i+1
  }
  
  # compute sensitivity, specificity, F1
  sen<-c(sen,round(TP/(TP+FN),digit=2))
  sep<-c(sep,round(TN/(TN+FP),digit=2))
  f1<-c(f1,round(2*TP/(2*TP+FP+FN),digit=2))
  
  # compute AUC
  
  if(target == "male"){
    eval <- prediction(d$pred.score,d$reference,c("female","male"))
  }else if(target == "female"){
    eval <- prediction(1-d$pred.score,d$reference,c("male","female"))
  }else{
    stop(paste("Unknown target: ", target), call.=FALSE)
  }
  
  value<-attributes(performance(eval,'auc'))$y.values[[1]]
  auc<-c(auc,round(value,digit=2))
}
out_data<-data.frame(method=ms, sensitivity=sen, specificity=sep, F1=f1, AUC=auc, stringsAsFactors = F)
index<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], which.max)

# output file
out_data<-rbind(out_data,c("highest",ms[index]))
write.table(out_data, file=out_f, quote = F, sep = ",", row.names=F)
