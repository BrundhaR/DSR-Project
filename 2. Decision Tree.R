library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)

#old_data  <- read.csv("/Users/brundhar/Desktop/DSR Project/creditcard.csv")   #for all 2.8L records
data  <- read.csv("/Users/brundhar/Desktop/DSR Project/Shortcreditcard.csv")    #for 20k records

NewData<-data[,-c(1)]

#DECISION TREE
start_time_d <- Sys.time()
decisionTree_model <- rpart(Class ~ . , NewData, method = 'class', control=rpart.control(minsplit=1), parms=list(split='information'))
predicted_val <- predict(decisionTree_model, NewData, type = 'class')
probability <- predict(decisionTree_model, NewData, type = 'prob')
rpart.plot(decisionTree_model, type=2)
end_time_d <- Sys.time()

cat ("Time taken to build model:", (end_time - start_time))

exit<-data$Class
var2<-cbind.data.frame( NewData$Time, NewData$Amount, exit )

pred<-predict(Logistic_Model,type="response")
predobj<-prediction(pred,var2$exit)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],4)))
aucobj@y.values
