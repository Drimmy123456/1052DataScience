library(shiny)
library('ROCR')
library(ggvis)
hw2<-function(target,files){
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
      pre<-d$pred.score
      eval <- prediction(pre,d$reference,c("female","male"))
    }else if(target == "female"){
      pre<-1-d$pred.score
      eval <- prediction(pre,d$reference,c("male","female"))
    }else{
      stop(paste("Unknown target: ", target), call.=FALSE)
    }
    
    value<-attributes(performance(eval,'auc'))$y.values[[1]]
    auc<-c(auc,round(value,digit=2))
  }
  out_data<-data.frame(method=ms, sensitivity=sen, specificity=sep, F1=f1, AUC=auc, stringsAsFactors = F)
  index<-sapply(out_data[,c("sensitivity","specificity","F1","AUC")], which.max)
  
  # output file
 # out_data<-rbind(out_data,c("highest",ms[index]))
  
  
  #list(out_data,out_measure)
  out_data
  
}
ROC<-function(target_2,files_2,threshold){
  # read data
  d<-read.table(files_2, header=T,sep=",")
  
  ## by threshold
  d$prediction <- sapply(d$pred.score,function(x){
    if(x>threshold){
      return("male")
    }else{
      return("female")
    }
  })
  
  ## label
  labels <- sapply(d$reference,function(x){
    if(x == "male"){
      return(1)
    }else if(x == "female"){
      return(0)
    }
  })
  
  ## target change
  if(target_2 == "female"){
    d$pred.score <- 1-d$pred.score
    labels <- 1-labels
  }
  
  
  # compute TP,TN,FP,FN
  TP<-0
  TN<-0
  FP<-0
  FN<-0
  i<-1 
  while(i < length(d$reference)){
    if(d$prediction[i]==target_2){
      if(d$reference[i]==target_2){
        TP<-TP+1
      }else{
        FP<-FP+1
      }
    }else{
      if(d$reference[i]==target_2){
        FN<-FN+1
      }else{
        TN<-TN+1
      }
    }
    i<-i+1
  }
  
  # compute sensitivity, specificity, F1
  sen<-round(TP/(TP+FN),digit=2)
  sep<-round(TN/(TN+FP),digit=2)
  f1<-round(2*TP/(2*TP+FP+FN),digit=2)
  
  # compute AUC
  eval <- prediction(d$pred.score,labels)
  value<-attributes(performance(eval,'auc'))$y.values[[1]]
  auc<-round(value,digit=2)
  
  out_data<-data.frame(method=gsub(".csv", "", basename(files_2)), sensitivity=sen, specificity=sep, F1=f1, AUC=auc, stringsAsFactors = F)
  
  perf <- attributes(performance(eval,"tpr","fpr"))
  roc_data <- data.frame(fpr = perf$x.values[[1]],tpr = perf$y.values[[1]])
  #,row.names = c("False positive rate","True positive rate","Cutoff")
  return(list(out_data,d,roc_data,sen,1-sep))
  
}

shinyServer(function(input, output) {
  
  result <- reactive({hw2(input$target,input$files)})
  
  ## ggvis plot
  reactive({
    ## function to show tooltip
    show_value <- function(x) {
      if(is.null(x)) return(NULL)
      paste0(format(x[1]), ": ", format(x[3]))
    }
    
    ## pipe line
    if(input$measure == 1){
      result %>%ggvis(x = ~method, y = ~sensitivity) %>% layer_bars() %>% add_tooltip(show_value, "hover")
    }else if(input$measure == 2){
      result %>% ggvis(x = ~method, y = ~specificity) %>% layer_bars() %>% add_tooltip(show_value, "hover")
    }else if(input$measure == 3){
      result %>% ggvis(x = ~method, y = ~F1) %>% layer_bars() %>% add_tooltip(show_value, "hover")
    }else{
      result %>% ggvis(x = ~method, y = ~AUC) %>% layer_bars() %>% add_tooltip(show_value, "hover")
    }
  })%>% bind_shiny("plot", "plot_ui")
  
  ## result table
  output$result_table <- renderTable({
    result()
  })
  ######
  # 2nd page
  ######
  
  result_2 <- reactive({ROC(input$target_2,input$files_2,input$threshold)})
  output$result_table_2_1 <- renderTable({result_2()[1]})
  output$result_table_2_2 <- renderTable({result_2()[2]})
  
  
  ## plot
  reactive({
    sen <- as.numeric(result_2()[4])
    spe <- as.numeric(result_2()[5])
    result_2()[[3]] %>% ggvis(x = ~fpr, y = ~tpr) %>% layer_paths() %>% layer_points(x = spe, y = sen, fill := "red")
  })%>% bind_shiny("plot_2", "plot_ui_2")
  
})
