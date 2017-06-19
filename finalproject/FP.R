library('rpart')

# read data
d <- read.csv("bird.csv",header=T)

# random split data into 5 folds
d$fold <- sample(1:nrow(d))
d$fold <- floor((d$fold-1)*5/nrow(d))+1


mydata <- list()
p_vec <- c()
for(k in 1:5){
	# pick testing and training data
  testing <- subset(d, fold %in% c(k))
  training <- subset(d, fold %in% (1:5)[-k])
  
	
	# train decision tree model from training data
	model <- rpart(formula = training$type ~., data = training[,-c(1,13)])

	# predict the testing data
	p <- predict(model, testing[,-c(12)], type="class")
	
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
	
	mydata[[k]] <- dt
	
}


accuracy <- round(sum(mydata[[1]][7,7],mydata[[2]][7,7],mydata[[3]][7,7],mydata[[4]][7,7],mydata[[5]][7,7])/5, digits = 2)

null_accuracy <- round(max(table(d[,12])) / nrow(d), digits = 2)

