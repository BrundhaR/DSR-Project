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

NewData<-data[,-c(1)]

start_time <- Sys.time()     #start timer

#to fit logistic regression model
Logistic_Model=glm(Class~.,NewData,family=binomial(link = "logit"), maxit=100)
#Logistic_Model=bayesglm(Class~.,NewData,family=binomial(), maxit=100)
summary(Logistic_Model)
end_time <- Sys.time()      #stop timer

cat ("Time taken to build model:", (end_time - start_time))

plot(Logistic_Model)

exit<-data$Class
var2<-cbind.data.frame( NewData$Time, NewData$Amount, exit )

pred<-predict(Logistic_Model,type="response")
predobj<-prediction(pred,var2$exit)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],4)))
aucobj@y.values
