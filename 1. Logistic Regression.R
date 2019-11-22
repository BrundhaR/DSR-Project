library(ggplot2)
library(pROC)
library(caTools)
library(ROCR)

#data  <- read.csv("/Users/brundhar/Desktop/DSR Project/creditcard.csv")   #for all 2.8L records
data  <- read.csv("/Users/brundhar/Desktop/DSR Project/Shortcreditcard.csv")    #for 20k records

#DATA PREPERATION

#scaling formula=(value-mean)/standard deviation
data$Amount=scale(data$Amount)
data$Time=scale(data$Time)

set.seed(123)
data_sample = sample.split(data$Class,SplitRatio=0.80)
train_data = subset(data,data_sample==TRUE)
train_data = train_data[,-c(1)]
test_data = subset(data,data_sample==FALSE)
test_data_class=test_data$Class
test_data = test_data[,-c(1)]
dim(train_data)
dim(test_data)

start_time <- Sys.time()     #start timer

#to fit logistic regression model
Logistic_Model=glm(Class~.,train_data,family=binomial(link = "logit"), maxit=100)
#Logistic_Model=bayesglm(Class~.,NewData,family=binomial(), maxit=100)
summary(Logistic_Model)

end_time <- Sys.time()      #stop timer

cat ("Time taken to build model:", (end_time - start_time))

plot(Logistic_Model)

class<-test_data_class
var2<-cbind.data.frame( test_data$Time, test_data$Amount, class )

pred<-predict(Logistic_Model,newdata=test_data, type="response")
predobj<-prediction(pred,var2$class)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],4)))
aucobj@y.values
