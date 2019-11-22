library(ggplot2)
library(gbm, quietly=TRUE)
library(pROC)
library(ROCR)

#old_data  <- read.csv("/Users/brundhar/Desktop/DSR Project/creditcard.csv")   #for all 2.8L records
data  <- read.csv("/Users/brundhar/Desktop/DSR Project/Shortcreditcard.csv")    #for 20k records


#DATA PREPERATION

#scaling formula=(value-mean)/standard deviation
data$Amount=scale(data$Amount)
data$Time=scale(data$Time)

#NewData=data[,-c(1)]
#head(NewData)

#splitting into training and testing data sets
set.seed(123)
data_sample = sample.split(data$Class,SplitRatio=0.80)
train_data = subset(data,data_sample==TRUE)
train_data_class=train_data$Class
train_data = train_data[,-c(1)]
test_data = subset(data,data_sample==FALSE)[,-c(1)]
dim(train_data)
dim(test_data)


#GRADIENT BOOSTING
start_time_g <- Sys.time()
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)

# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")

model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
plot(model_gbm)

# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "red" )
print(gbm_auc)

end_time_g <- Sys.time()
cat ("Time taken to build model:", (end_time_g - start_time_g))

class<-test_data$Class
head(class)
var2<-cbind.data.frame( test_data$Time, test_data$Amount, class )
pred<-predict(Logistic_Model,type="response")


predobj<-prediction(gbm_test,var2$class)
rocobj<-performance(predobj,measure="tpr",x.measure="fpr")
aucobj<-performance(predobj,measure="auc")

plot(rocobj,main=paste("Area under curve",round(aucobj@y.values[[1]],5)))
aucobj@y.values


