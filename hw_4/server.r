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

shinyServer(function(input, output) {
  
  result <- reactive({hw2(input$target,input$files)})
#  measure <- reactive({input$measure})
  
  
#  if(measure == '1'){
#    result %>% ggvis(x = ~method, y = ~sensitivity) %>% layer_bars() %>% bind_shiny("plot", "plot_ui")
#  }else if(input$measure == 2){
#    result %>% ggvis(x = ~method, y = ~specificity) %>% layer_bars() %>% bind_shiny("plot", "plot_ui")
#  }else if(input$measure == 3){
#    result %>% ggvis(x = ~method, y = ~F1) %>% layer_bars() %>% bind_shiny("plot", "plot_ui")
#  }else{
#    result %>% ggvis(x = ~method, y = ~AUC) %>% layer_bars() %>% bind_shiny("plot", "plot_ui")
#  }
#  measure <- 1
  
  result %>% ggvis(x = ~method, y = ~sensitivity) %>% layer_bars() %>% bind_shiny("plot1", "plot_ui")
  result %>% ggvis(x = ~method, y = ~specificity) %>% layer_bars() %>% bind_shiny("plot2", "plot_ui")
  result %>% ggvis(x = ~method, y = ~F1) %>% layer_bars() %>% bind_shiny("plot3", "plot_ui")
  result %>% ggvis(x = ~method, y = ~AUC) %>% layer_bars() %>% bind_shiny("plot4", "plot_ui")
  
  
  output$result_table <- renderTable({
    result()
  })
  
  
  
})
