library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)

#old_data  <- read.csv("/Users/brundhar/Desktop/DSR Project/creditcard.csv")   #for all 2.8L records
data  <- read.csv("/Users/brundhar/Desktop/DSR Project/Shortcreditcard.csv")    #for 20k records

NewData<-data[,-c(1)]

set.seed(123)
data_sample = sample.split(data$Class,SplitRatio=0.80)
train_data = subset(data,data_sample==TRUE)
train_data = train_data[,-c(1)]
test_data = subset(data,data_sample==FALSE)
test_data_class=test_data$Class
test_data = test_data[,-c(1)]
dim(train_data)
dim(test_data)

#DECISION TREE
start_time_d <- Sys.time()
decisionTree_model <- rpart(Class ~ . , train_data, method = 'class', control=rpart.control(minsplit=1), parms=list(split='information'))
predicted_val <- predict(decisionTree_model, train_data, type = 'class')
probability <- predict(decisionTree_model, train_data, type = 'prob')
rpart.plot(decisionTree_model, type=2)
end_time_d <- Sys.time()

cat ("Time taken to build model:", (end_time_d - start_time_d))

class<-test_data_class
head(class)
var2<-cbind.data.frame( test_data$Time, test_data$Amount, class )
pred<-predict(decisionTree_model,newdata=test_data, type="prob")[,2]

predobj<-prediction(pred,var2$class)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],6)))
aucobj@y.values



