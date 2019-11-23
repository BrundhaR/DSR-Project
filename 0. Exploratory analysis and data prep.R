library(ggplot2)

#old_data  <- read.csv("/Users/brundhar/Desktop/creditcard.csv")   #for all 2.8L records
old_data  <- read.csv("/Users/brundhar/Desktop/Shortcreditcard.csv")    #for 20k records
data <- old_data[c(0:20000),]


#EXPLORATORY ANALYSIS
str(data)
summary(data)
head(data)

#trends of data vectors
barplot(table(data$Amount), xlab="Amount", main="Distribution of Amount")
barplot(table(data$Time), xlab="Time", main="Distribution of Time Feature")
barplot(table(data$Class), xlab="Class (0:Non-Fraudulent, 1:Fraudulent)", ylab="Count", main="Distribution of Class Feature")



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

#to show trends in training data
barplot(table(train_data$Amount), xlab="Amount", main="Distribution of Amount")
barplot(table(train_data$Time), xlab="Time", main="Distribution of Time Feature")
barplot(table(train_data$Class), xlab="Class (0:Non-Fraudulent, 1:Fraudulent)", ylab="Count", main="Distribution of Class Feature")

#to show trends in testing data
barplot(table(test_data$Amount), xlab="Amount", main="Distribution of Amount")
barplot(table(test_data$Time), xlab="Time", main="Distribution of Time Feature")
barplot(table(test_data$Class), xlab="Class (0:Non-Fraudulent, 1:Fraudulent)", ylab="Count", main="Distribution of Class Feature")

