library(ggplot2)
library(caTools)
library(neuralnet)
library(pROC)
library(ROCR)

#old_data  <- read.csv("/Users/brundhar/Desktop/DSR Project/creditcard.csv")   #for all 2.8L records
data  <- read.csv("/Users/brundhar/Desktop/DSR Project/Shortcreditcard.csv")    #for 20k records


#DATA PREPERATION

#scaling formula=(value-mean)/standard deviation
data$Amount=scale(data$Amount)
data$Time=scale(data$Time)

#splitting into training and testing data sets
set.seed(123)
data_sample = sample.split(data$Class,SplitRatio=0.80)
train_data = subset(data,data_sample==TRUE)
train_data = train_data[,-c(1)]
test_data = subset(data,data_sample==FALSE)
test_data_class=test_data$Class
test_data = test_data[,-c(1)]
dim(train_data)
dim(test_data)

#ARTIFICIAL NEURAL NETWORK
start_time <- Sys.time()
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)
resultANN
end_time <- Sys.time()
cat ("Time taken to build model:", (end_time - start_time))

class<-test_data_class
head(class)
var2<-cbind.data.frame( test_data$Time, test_data$Amount, class )
pred<-predict(ANN_model,newdata=test_data, type="response")


predobj<-prediction(pred,var2$class)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],6)))
aucobj@y.values



