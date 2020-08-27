#Titanic
library(rpart)
library(rpart.plot)
library(titanic)
library(ROCR)
my_titanic <- rpart(Survived~Age+Sex+Pclass+SibSp+Fare,data=titanic_train,method='class')
rpart.plot(my_titanic,extra=1,type=1)

####clean ger_credit
library(readxl)
mydf<- read_excel("german credit card.xls")
View(mydf)
#rub data, replace missing values X
mydf$purpose_num<-as.numeric(replace(mydf$purpose, "", "X"))[1:1000]

#rub data, transform good_bad to 1&0 and numeric
mydf$good_bad<-gsub(mydf$good_bad,pattern='good',replacement=1)
mydf$good_bad<-gsub(mydf$good_bad,pattern='bad',replacement=0)

mydf$good_bad<-as.numeric(mydf$good_bad)

ger_tree <- rpart(good_bad~age+amount+duration+checking,data=mydf,method='class')
rpart.plot(ger_tree,extra=1,type=1)
ger_logit <-glm(good_bad~age+duration+checking,data=mydf,family='binomial')
summary(ger_logit)
# compare models:
#tree: amount is not the most important, but it matters after checking and duration
#logistic: amount does not matter
#tree: age doesn't matter
#logit: age is a significant variable

#check how data insight relate to model performance: which one is better
predict_tree<-predict(ger_tree,mydf,type='prob')
predict_logit<-predict(ger_logit,mydf,type='response')
predict_val_tree<-prediction(predict_tree[,2],mydf$good_bad) 
# [,2] the second variable bc we have a dataframe with 2 variables, prob 0 and 1. 1 is the sucess.
predict_val_logit<-prediction(predict_logit,mydf$good_bad)

perf_tree<-performance(predict_val_tree,'tpr','fpr')
perf_logit<-performance(predict_val_logit,'tpr','fpr')
plot(perf_tree,col='black')
plot(perf_logit,col='red',add=T)
#both models are good. if fpr is lower, logit model is better. for high fpr, the tree is better
#in real world, normally interested in low fpr. in that case logit model wins.

#perform data model comparison on FB dataset
library(readr)
dataset_Facebook <- read_csv("dataset_Facebook.csv")
View(dataset_Facebook)

anyNA(dataset_Facebook)
summary(dataset_Facebook)
#NAs: like 1, paid 1, share 4. impute with 0
dataset_Facebook$like[which(is.na(dataset_Facebook$like))]<-0
dataset_Facebook$share[which(is.na(dataset_Facebook$share))]<-0
dataset_Facebook$Paid[which(is.na(dataset_Facebook$Paid))]<-0
anyNA(dataset_Facebook)

#check if Paid is numeric. Yes it is.
class(dataset_Facebook$Paid)

fb_tree <- rpart(Paid~comment+like+share,data=dataset_Facebook,method='class')
rpart.plot(fb_tree,extra=1,type=1)
fb_logit <-glm(Paid~comment+like+share,data=dataset_Facebook,family='binomial')
summary(fb_logit)

predict_tree<-predict(fb_tree,dataset_Facebook,type='prob')
predict_logit<-predict(fb_logit,dataset_Facebook,type='response')
predict_val_tree<-prediction(predict_tree[,2],dataset_Facebook$Paid) 
predict_val_logit<-prediction(predict_logit,dataset_Facebook$Paid)

perf_tree<-performance(predict_val_tree,'tpr','fpr')
perf_logit<-performance(predict_val_logit,'tpr','fpr')
plot(perf_tree,col='black')
plot(perf_logit,col='red',add=T)
#for lower fpr, tree model is better 
t<-plot_ly(data=dataset_Facebook,x=~like, y=~Paid)
t

p <-ggplot(dataset_Facebook,aes(x=like,y=Paid))+
  geom_jitter(aes(color=Category))
ggplotly(p)
z<-plot_ly(data=dataset_Facebook,x=~like,y=~Paid,z=~comment)
z
######
#see class 9 r markdown

#plotly
install.packages('plotly')
library(plotly)
t<-plot_ly(data=mydf,x=~age, y=~good_bad)
t

p <-ggplot(mydf,aes(x=age,y=good_bad))+
  geom_jitter(aes(color=checking))
ggplotly(p)

#more poeple good than bad, less old people has checking accounts
#use selection, grey out the rest points

#3d plots are hard to read, but better in plotly
z<-plot_ly(data=mydf,x=~duration,y=~age,z=~amount,color=~good_bad,
           type='scatter3d',mode='markers')
z
#you can modify chart,spin and examine clusters in three dimensions. There 
# is no correlation in 2d, but there is in 3d. 
#Darker s are outside on the plot.