

# DECISION TREES AND RANDOM FORESTS

install.packages("gmodels")
library(gmodels)	
install.packages("e1071")
library(e1071)
install.packages("caret")
library(caret)
install.packages("readr")
library(readr)
install.packages("ROCR")
library(ROCR)
install.packages("c50")
library(C50)
install.packages("randomForest")
library(randomForest)

complaints_random <- read_csv("Consumer_Complaints.csv")
complaints_random <-as.data.frame(complaints_random)
attach(complaints_random)

complaints_random$Issue<-as.factor(complaints_random$Issue)
complaints_random$Product<-as.factor(complaints_random$Product)
complaints_random$Company<-as.factor(complaints_random$Company)
complaints_random$State<-as.factor(complaints_random$State)
complaints_random$`Submitted via`<-as.factor(complaints_random$`Submitted via`)
complaints_random$`Timely response?`<-as.factor(complaints_random$`Timely response?`)
complaints_random$`Consumer disputed?`<-as.factor(complaints_random$`Consumer disputed?`)
complaints_random$`Company response to consumer`<-as.factor(complaints_random$`Company response to consumer`)


complaints_random<-complaints_random[,-2]
complaints_random<-complaints_random[,-3]

nrow(complaints_random)
complaints_random<-na.omit(complaints_random)


set.seed((4589))
no<-nrow(complaints_random)
rand<-complaints_random[order(runif(no)),]
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



