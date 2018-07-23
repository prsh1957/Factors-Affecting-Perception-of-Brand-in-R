setwd("C:\\PRSH Downloads\\JIGSAW\\Course Topics_R\\Logistic Regression Models")
library(dplyr)
library(caret)

data<-read.csv("goodforu.csv",sep=",")
summary(data)
str(data)

#let us select important columns to understand perception on Brand A
data%>%select(X23,X2,X9,X16,X30)->data1

plot(as.factor(data1$X30),data1$X23,col="red")

data2<-data1

#let us covert X23 into target (binary) variable
data2$Target<-ifelse(data2$X23>4,1,0)
data2%>%select(-X23)->data2

#Let us split the data into training and test datasets
set.seed(400)
index<-sample(nrow(data2),0.70*nrow(data2),replace=F)
train<-data2[index,]
test<-data2[-index,]

#Dummy Variable X2(yes)-just for confirming the answer of Que8 
train$X2_d<-ifelse(train$X2==1,1,0)
test$X2_d<-ifelse(test$X2==1,1,0)


#Let us build the initial model
model<-glm(Target~X2+X9+X16+X30,data=train,family="binomial")
summary(model)

#predicted probabilities
pred<-predict(model,type="response",newdata=test)
pred1<-ifelse(pred>=0.4957286,1,0)


pred1<-as.factor(pred1)
test$Target1<-as.factor(test$Target)
confusionMatrix(pred1,test$Target1,positive = "1")

#Let us see which combinations occur the most for good perception by predicted probabilites
test1<-test
test1$pred<-pred
test1<-arrange(test1,-pred)
test1%>%select(-Target,-Target1)->final
final$comb<-paste(as.character(final$X2),as.character(final$X9),as.character(final$X16),as.character(final$X30),sep='')
final%>%group_by(comb)%>%summarize(mean(pred))->final

#ROCR curve
library(ROCR)
pred1<-as.numeric(as.character(pred1))
actual<-test$Target
pred<-prediction(actual,pred1)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
auc<-performance(pred,"auc")
unlist(auc@y.values)


