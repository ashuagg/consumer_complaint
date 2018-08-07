# NAIVE BAYES
# Question 1: Predicting the response to a particular consumer complaint

# Loading the libraries and the dataset

library(gmodels)	
library(e1071)
library(caret)
library(readr)
library(ROCR)

attach(complaints)

# ensuring all categorical values as treated as factors
complaints$Issue<-factor(complaints$Issue)
complaints$Product<-factor(complaints$Product)
complaints$Company<-factor(complaints$Company)
complaints$State<-factor(complaints$State)
complaints$`Submitted via`<-factor(complaints$`Submitted via`)
complaints$`Timely response?`<-factor(complaints$`Timely response?`)
complaints$`Consumer disputed?`<-factor(complaints$`Consumer disputed?`)
complaints$`Company response to consumer`<-factor(complaints$`Company response to consumer`)

# creating training and testing datasets (75:25)
set.seed(12345)
indx<-createDataPartition(complaints$`Company response to consumer`, p=0.75, list = FALSE)
train<-complaints[indx,]
test<-complaints[-indx,]

# building the Naive Bayes model
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

# creating the confusion table
xtab<-table(nb_classifier_pred,test$`Company response to consumer`)
confusionMatrix(xtab)

#Laplace

# building the Naive Bayes model with laplace=1
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 1)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# building the Naive Bayes model with laplace=2
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 2)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)

# Sub-sampling
# Creating subsets for sampling

closed<-subset(complaints,`Company response to consumer`=="Closed")
closed_exp<-subset(complaints,`Company response to consumer`=="Closed with explanation")
closed_mon<-subset(complaints,`Company response to consumer`=="Closed with monetary relief")
closed_no_mon<-subset(complaints,`Company response to consumer`=="Closed with non-monetary relief")
closed_mon<-subset(complaints,`Company response to consumer`=="Closed with monetary relief")
closed_relief<-subset(complaints, `Company response to consumer`=="Closed with relief")
closed_no_relief<-subset(complaints, `Company response to consumer`=="Closed without relief")

set.seed(748)
n<-nrow(closed_relief)

rand<-closed[order(runif(n)),]
d<-rbind(closed_relief,rand)

rand<-closed_exp[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_mon[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_no_mon[order(runif(n)),]
d<-rbind(d,rand)

rand<-closed_no_relief[order(runif(n)),]
d<-rbind(d,rand)

# creating training and testing datasets (75:25)
set.seed((37543))
no<-nrow(d)
rand<-d[order(runif(no)),]
splt<-(no*0.75)
train<-rand[1:splt,]
test<-rand[(splt+1):no,]

# building the Naive Bayes model
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

# creating the confusion table
xtab<-table(nb_classifier_pred,test$`Company response to consumer`)
confusionMatrix(xtab)

#Laplace

# building the Naive Bayes model with laplace=1
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 1)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# building the Naive Bayes model with laplace=2
nb_classifier<-	naiveBayes(as.factor(train$`Company response to consumer`)~.,data=train, laplace = 2)

# Testing the model on testing data
nb_classifier_pred<-predict(nb_classifier,newdata = test)

xtab<-table(nb_classifier_pred,test$`Company response to consumer`)

# creating the confusion table
confusionMatrix(xtab)


# DECISION TREES AND RANDOM FORESTS

library(gmodels)	
library(e1071)
library(caret)
library(readr)
library(ROCR)
library(C50)
library(randomForest)

complaints <- read_csv("C:/Users/paldo/Desktop/MIM/Spring 2017/INST737/Project/Clean_Consumer_Complaints.csv")
complaints <-as.data.frame(complaints)
attach(complaints)

complaints$Issue<-as.factor(complaints$Issue)
complaints$Product<-as.factor(complaints$Product)
complaints$Company<-as.factor(complaints$Company)
complaints$State<-as.factor(complaints$State)
complaints$`Submitted via`<-as.factor(complaints$`Submitted via`)
complaints$`Timely response?`<-as.factor(complaints$`Timely response?`)
complaints$`Consumer disputed?`<-as.factor(complaints$`Consumer disputed?`)
complaints$`Company response to consumer`<-as.factor(complaints$`Company response to consumer`)


complaints<-complaints[,-2]
complaints<-complaints[,-3]

nrow(complaints)
complaints<-na.omit(complaints)


set.seed((4589))
no<-nrow(complaints)
rand<-complaints[order(runif(no)),]
splt<-(no*0.75)
train<-rand[1:splt,]
test<-rand[(splt+1):no,]

prop.table(table(train$`Company response to consumer`))
prop.table(table(test$`Company response to consumer`))

model<-C50::C5.0(train[-4],train$`Company response to consumer`)
model
summary(model)
pred<-predict(model,test)

xtab<-table(pred,test$`Company response to consumer`)
a<-confusionMatrix(xtab)
a$overall['Accuracy']

#boosting
model_boost<-C50::C5.0(train[-4],train$`Company response to consumer`,trials=10)
model_boost
summary(model_boost)
pred_boost<-predict(model_boost,test)

xtab_boost<-table(pred_boost,test$`Company response to consumer`)
a<-confusionMatrix(xtab_boost)
a$overall['Accuracy']

# boosting
# model_bag<-randomForest(train$`Company response to consumer`~.,data=train, mtry=13, importance=TRUE)


