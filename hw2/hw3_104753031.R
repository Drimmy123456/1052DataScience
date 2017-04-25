library('ROCR')
# defind function: which.second(x)
which.second<-function(x){
  sort.int(x,decreasing = TRUE,index.return = TRUE)$ix[2]
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw3_104753031.R --target male/female --files meth1 meth2 ... methx --out result.csv", call.=FALSE)
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
correct<-c()
incorrect<-c()

for(file in files)
{
  # read data
  d<-read.table(file, header=T,sep=",")
  
  # record method name
  name<-gsub(".csv", "", basename(file))
  ms<-c(ms,name)
  
  # compute TP,TN,FP,FN
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  i<-1 
  while(i < length(d$reference)){
    if(d$prediction[i]==target){
      if(d$reference[i]==target){
        TP<-TP+1
      }else{
        FP<-FP+1
      }
    }else{
      if(d$reference[i]==target){
        FN<-FN+1
      }else{
        TN<-TN+1
      }
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
  
  # collect the number of correct
  correct<-c(correct,TP+TN)
  incorrect<-c(incorrect,FP+FN)

}
out_data<-data.frame(method=ms, sensitivity=sen, specificity=sep, F1=f1, AUC=auc, stringsAsFactors = F)

# find index of the max and the second value to each score
index_max<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], which.max)
index_second<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], which.second)

print(index_max)
print(index_second)

# contingency table and significance test
star<-c()
i<-1
while(i < 5){
  d <- data.frame(c(correct[[index_max[[i]]]],correct[[index_second[[i]]]]),c(incorrect[[index_max[[i]]]]),incorrect[[index_second[[i]]]])

  
  
  if(fisher.test(d)$p.value<0.05){
    star<-c(star,TRUE)
  }else{
    star<-c(star,FALSE)
  }
  
  print(fisher.test(d)$p.value)
  
  i<-i+1
}



# output file
last_row<-c("highest",ms[index_max])
i<-2
while(i < 6){
  if(star[i-1]==TRUE){
    last_row[i]<-paste0(last_row[i],"*")
  }
  
  i<-i+1
}

out_data<-rbind(out_data,last_row)
write.table(out_data, file=out_f, quote = F, sep = ",", row.names=F)

