library(shiny)
library(rpart)
library(rpart.plot)

shinyServer(function(input, output) {
  d <- reactive({
    d <- read.csv("bird.csv",header=T)
    
    #random split data into 5 folds
    d$fold <- sample(1:nrow(d))
    d$fold <- floor((d$fold-1)*5/nrow(d))+1
    
    d
  })
  
  ###### k = 1
  
  # construct model
  model1 <- reactive({
    # collect training data
    training <- subset(d(), fold %in% (1:5)[-1])
    
    # train decision tree model from training data
    rpart(formula = training$type ~., data = training[,-c(1,13)])
  })
  
  # a table records CM, sensitivity, precision, and accuracy
  result1 <- reactive({
    # collect testing data
    testing <- subset(d(), fold %in% c(1))
    
    # predict the testing data
    p <- predict(model1(), testing[,-c(12)], type="class")
    # compute CM 
    t <- table(act = testing$type,pred = p)
    dt<-as.data.frame.matrix(t)
    dt[] <- lapply(dt, as.character)
    
    # append sensitivity
    dt <- cbind(dt,sensitivity = c(round(t[1,1]/sum(t[1,]),digits = 2),
                                   round(t[2,2]/sum(t[2,]),digits = 2),
                                   round(t[3,3]/sum(t[3,]),digits = 2),
                                   round(t[4,4]/sum(t[4,]),digits = 2),
                                   round(t[5,5]/sum(t[5,]),digits = 2),
                                   round(t[6,6]/sum(t[6,]),digits = 2)))
    
    # append precision and accuracy (at [7,7])
    dt <- rbind(dt,precision = c(round(t[1,1]/sum(t[,1]),digits = 2),
                                 round(t[2,2]/sum(t[,2]),digits = 2),
                                 round(t[3,3]/sum(t[,3]),digits = 2),
                                 round(t[4,4]/sum(t[,4]),digits = 2),
                                 round(t[5,5]/sum(t[,5]),digits = 2),
                                 round(t[6,6]/sum(t[,6]),digits = 2),
                                 round(length(which(p == testing[, 12] )) / nrow(testing), digits = 2)))
    
    dt
  })
  
  output$plot1 <- renderPlot({
    plot(model1())
    text(model1())
  })
  
  output$table1 <-renderTable(rownames = TRUE,{
    result1()
  })
  
  ###### k = 2
  
  # construct model
  model2 <- reactive({
    # collect training data
    training <- subset(d(), fold %in% (1:5)[-2])
    
    # train decision tree model from training data
    rpart(formula = training$type ~., data = training[,-c(1,13)])
  })
  
  # a table records CM, sensitivity, precision, and accuracy
  result2 <- reactive({
    # collect testing data
    testing <- subset(d(), fold %in% c(2))
    
    # predict the testing data
    p <- predict(model2(), testing[,-c(12)], type="class")
    # compute CM 
    t <- table(act = testing$type,pred = p)
    dt<-as.data.frame.matrix(t)
    dt[] <- lapply(dt, as.character)
    
    # append sensitivity
    dt <- cbind(dt,sensitivity = c(round(t[1,1]/sum(t[1,]),digits = 2),
                                   round(t[2,2]/sum(t[2,]),digits = 2),
                                   round(t[3,3]/sum(t[3,]),digits = 2),
                                   round(t[4,4]/sum(t[4,]),digits = 2),
                                   round(t[5,5]/sum(t[5,]),digits = 2),
                                   round(t[6,6]/sum(t[6,]),digits = 2)))
    
    # append precision and accuracy (at [7,7])
    dt <- rbind(dt,precision = c(round(t[1,1]/sum(t[,1]),digits = 2),
                                 round(t[2,2]/sum(t[,2]),digits = 2),
                                 round(t[3,3]/sum(t[,3]),digits = 2),
                                 round(t[4,4]/sum(t[,4]),digits = 2),
                                 round(t[5,5]/sum(t[,5]),digits = 2),
                                 round(t[6,6]/sum(t[,6]),digits = 2),
                                 round(length(which(p == testing[, 12] )) / nrow(testing), digits = 2)))
    
    dt
  })
  
  output$plot2 <- renderPlot({
    plot(model2())
    text(model2())
  })
  
  output$table2 <-renderTable(rownames = TRUE,{
    result2()
  })
  
  ###### k = 3
  
  # construct model
  model3 <- reactive({
    # collect training data
    training <- subset(d(), fold %in% (1:5)[-3])
    
    # train decision tree model from training data
    rpart(formula = training$type ~., data = training[,-c(1,13)])
  })
  
  # a table records CM, sensitivity, precision, and accuracy
  result3 <- reactive({
    # collect testing data
    testing <- subset(d(), fold %in% c(3))
    
    # predict the testing data
    p <- predict(model3(), testing[,-c(12)], type="class")
    # compute CM 
    t <- table(act = testing$type,pred = p)
    dt<-as.data.frame.matrix(t)
    dt[] <- lapply(dt, as.character)
    
    # append sensitivity
    dt <- cbind(dt,sensitivity = c(round(t[1,1]/sum(t[1,]),digits = 2),
                                   round(t[2,2]/sum(t[2,]),digits = 2),
                                   round(t[3,3]/sum(t[3,]),digits = 2),
                                   round(t[4,4]/sum(t[4,]),digits = 2),
                                   round(t[5,5]/sum(t[5,]),digits = 2),
                                   round(t[6,6]/sum(t[6,]),digits = 2)))
    
    # append precision and accuracy (at [7,7])
    dt <- rbind(dt,precision = c(round(t[1,1]/sum(t[,1]),digits = 2),
                                 round(t[2,2]/sum(t[,2]),digits = 2),
                                 round(t[3,3]/sum(t[,3]),digits = 2),
                                 round(t[4,4]/sum(t[,4]),digits = 2),
                                 round(t[5,5]/sum(t[,5]),digits = 2),
                                 round(t[6,6]/sum(t[,6]),digits = 2),
                                 round(length(which(p == testing[, 12] )) / nrow(testing), digits = 2)))
    
    dt
  })
  
  output$plot3 <- renderPlot({
    plot(model3())
    text(model3())
  })
  
  output$table3 <-renderTable(rownames = TRUE,{
    result3()
  })
  
  ###### k = 4
  
  # construct model
  model4 <- reactive({
    # collect training data
    training <- subset(d(), fold %in% (1:5)[-4])
    
    # train decision tree model from training data
    rpart(formula = training$type ~., data = training[,-c(1,13)])
  })
  
  # a table records CM, sensitivity, precision, and accuracy
  result4 <- reactive({
    # collect testing data
    testing <- subset(d(), fold %in% c(4))
    
    # predict the testing data
    p <- predict(model4(), testing[,-c(12)], type="class")
    # compute CM 
    t <- table(act = testing$type,pred = p)
    dt<-as.data.frame.matrix(t)
    dt[] <- lapply(dt, as.character)
    
    # append sensitivity
    dt <- cbind(dt,sensitivity = c(round(t[1,1]/sum(t[1,]),digits = 2),
                                   round(t[2,2]/sum(t[2,]),digits = 2),
                                   round(t[3,3]/sum(t[3,]),digits = 2),
                                   round(t[4,4]/sum(t[4,]),digits = 2),
                                   round(t[5,5]/sum(t[5,]),digits = 2),
                                   round(t[6,6]/sum(t[6,]),digits = 2)))
    
    # append precision and accuracy (at [7,7])
    dt <- rbind(dt,precision = c(round(t[1,1]/sum(t[,1]),digits = 2),
                                 round(t[2,2]/sum(t[,2]),digits = 2),
                                 round(t[3,3]/sum(t[,3]),digits = 2),
                                 round(t[4,4]/sum(t[,4]),digits = 2),
                                 round(t[5,5]/sum(t[,5]),digits = 2),
                                 round(t[6,6]/sum(t[,6]),digits = 2),
                                 round(length(which(p == testing[, 12] )) / nrow(testing), digits = 2)))
    
    dt
  })
  
  output$plot4 <- renderPlot({
    plot(model4())
    text(model4())
  })
  
  output$table4 <-renderTable(rownames = TRUE,{
    result4()
  })
  
  ###### k = 5
  
  # construct model
  model5 <- reactive({
    # collect training data
    training <- subset(d(), fold %in% (1:5)[-5])
    
    # train decision tree model from training data
    rpart(formula = training$type ~., data = training[,-c(1,13)])
  })
  
  # a table records CM, sensitivity, precision, and accuracy
  result5 <- reactive({
    # collect testing data
    testing <- subset(d(), fold %in% c(5))
    
    # predict the testing data
    p <- predict(model5(), testing[,-c(12)], type="class")
    # compute CM 
    t <- table(act = testing$type,pred = p)
    dt<-as.data.frame.matrix(t)
    dt[] <- lapply(dt, as.character)
    
    # append sensitivity
    dt <- cbind(dt,sensitivity = c(round(t[1,1]/sum(t[1,]),digits = 2),
                                   round(t[2,2]/sum(t[2,]),digits = 2),
                                   round(t[3,3]/sum(t[3,]),digits = 2),
                                   round(t[4,4]/sum(t[4,]),digits = 2),
                                   round(t[5,5]/sum(t[5,]),digits = 2),
                                   round(t[6,6]/sum(t[6,]),digits = 2)))
    
    # append precision and accuracy (at [7,7])
    dt <- rbind(dt,precision = c(round(t[1,1]/sum(t[,1]),digits = 2),
                                 round(t[2,2]/sum(t[,2]),digits = 2),
                                 round(t[3,3]/sum(t[,3]),digits = 2),
                                 round(t[4,4]/sum(t[,4]),digits = 2),
                                 round(t[5,5]/sum(t[,5]),digits = 2),
                                 round(t[6,6]/sum(t[,6]),digits = 2),
                                 round(length(which(p == testing[, 12] )) / nrow(testing), digits = 2)))
    
    dt
  })
  
  output$plot5 <- renderPlot({
    plot(model5())
    text(model5())
  })
  
  output$table5 <-renderTable(rownames = TRUE,{
    result5()
  })
  
  
  
  # average accuracy
  output$accuracy <- renderText({
    round(sum(result1()[7,7],result2()[7,7],result3()[7,7],result4()[7,7],result5()[7,7])/5, digits = 2)
  })
  
  # null model accuracy
  output$null_accuracy <- renderText({
    round(max(table(d()[,12])) / nrow(d()), digits = 2)
  })
  
  
})
