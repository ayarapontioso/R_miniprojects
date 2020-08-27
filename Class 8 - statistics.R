library(readxl)
mydf<- read_excel("german credit card.xls")
View(mydf)
#rub data, replace missing values X
mydf$purpose_num<-as.numeric(replace(mydf$purpose, "", "X"))[1:1000]

#rub data, transform good_bad to 1&0 and numeric
mydf$good_bad<-gsub(mydf$good_bad,pattern='good',replacement=1)
mydf$good_bad<-gsub(mydf$good_bad,pattern='bad',replacement=0)

mydf$good_bad<-as.numeric(mydf$good_bad)

#subset good bad
mydf_good <- mydf[which(mydf$good_bad == 1),c('checking','duration')]
mydf_bad <- mydf[which(mydf$good_bad == 0),1:2]

#single variable regression
my_ger<-glm(good_bad~age,data=mydf,family = 'binomial')
summary(my_ger)

# a model with more variables
my_ger_bigger <- glm(good_bad~age+amount+duration+savings+telephon,data=mydf, family="binomial")
summary(my_ger_bigger)
#remove amount, telephon, the irrelevant factors
#this model is better, we included more information, the model trained better

# two ways to compare models
# 1. akaike information: best if we have the same set of variables
# 2. deviance residuals: can compares all variables
  # Compare mediums. The smaller, the better. 
  # deviance residual: from a graphical standpoint, the small deviance from the squiggle to the 0/1
   #mathematically, the maximum likelihood

#build the smallest medium residual models, to build the best model for ger_cre
my_ger_bigger <- glm(good_bad~checking+history+duration+installp+marital+other+savings,data=mydf, family="binomial")
summary(my_ger_bigger)
#0.4588

#Iris
# what are the assumptions in the iris model in hw3: 
# 1. continuous
# 2. lack of multilinearity- one line fits all

plot(iris$Sepal.Length,iris$Petal.Length)  
#test for is my data look like the relation is in a pipe(homostatistic) or cone(heterostatistic)
#plot cannot be fit by one line, but can be fit using quartile regression
install.packages('quantreg')
library(quantreg)

#run the one line linear reg
lin_bad <- lm(Petal.Length~Sepal.Length, data = iris)
summary(lin_bad)
# slope 1.8 for one line

#run the better quartile reg
rq_model<-rq(Petal.Length~Sepal.Length, data = iris, tau=0.2) # tau<- quartile
summary(rq_model)  # b1=2.0
rq_model<-rq(Petal.Length~Sepal.Length, data = iris, tau=0.5) #50th quartile
summary(rq_model)  # b1=1.81
rq_model<-rq(Petal.Length~Sepal.Length, data = iris, tau=0.9) #90th quartile
summary(rq_model)  # b1=1.40 this is the most important quartile

#run for ger credit
rq_model<-rq(amount~age, data = mydf, tau=0.2) # tau<- quartile, around 25 yr
summary(rq_model)  # b1=-0.52
rq_model<-rq(amount~age, data = mydf, tau=0.5) #50th quartile, around 40 yr
summary(rq_model)  # b1=-1.28
rq_model<-rq(amount~age, data = mydf, tau=0.9) #90th quartile, around 75 yr
summary(rq_model)  # b1=24.92, this is the most important quartile
#insight: young customers, low quantile, every increase in age, decrease in amt
# older customers get older by 1yr, amount shoots up by $25

#run one line
lin_bad <- lm(amount~age, data = mydf)
summary(lin_bad)  #b1=8.1, one lines does not fit all

#Titanic
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
install.packages('titanic')
library(titanic)
summary(titanic_train)
mydf_train <- as.data.frame(titanic_train)
View(mydf_train)
logit_tit <- glm(Survived~Sex+Age+SibSp+Pclass+Fare, data=titanic_train, family='binomial')
summary(logit_tit)
#fare insig, delete
logit_tit <- glm(Survived~Sex+Age+SibSp+Pclass, data=titanic_train, family='binomial')
summary(logit_tit)
#insight if change sex from female to male, odds of survival goes down by
exp(-2.62)-1
#if change age to 1 yr older, odds of survival goes down by
exp(-0.044385)-1
#one more sibling
exp(-0.376)-1
#go down by 1 class, the survival odds goes down by
exp(-1.317398)-1

predict_logit <-predict(logit_tit, titanic_train, type='response')
predict_logit[1:5] #first 5 passengers
head(titanic_train,5)

#challenger model
tree_tit <- rpart(Survived~Sex+Age+SibSp+Pclass, data=titanic_train, method='class', cp=0.013)
#no summary, bc gives branches. instead, plot
rpart.plot(tree_tit,extra=1,type=1)

predict_tree <- predict(tree_tit, titanic_train, type='prob')
predict_tree[1:5,2]  #show the 2nd column
plotcp(tree_tit)  #smallest error 0.13, update tree tit cp to 0.13
#update to 0.0001 overfitting the data, too big a tree. takes a long time to interpret and explain
# to reduce the fit, the prune, to generalize. 
#pruning: cp =.1, not enough biz insight. 
# this tree is too small and general, we have to improve the fit
# Finally, cp=.013, the most optimal tree
#Note: don't get fixated on cp, visually tree could be too big or small. test to figure out.

install.packages('ROCR')
library(ROCR)
# ROCR has prediction function, use it to create challenger model
pred_val_tree<-prediction(predict_tree[,2],titanic_train$Survived)
# next, logistic
pred_val_logit <- prediction(predict_logit,titanic_train$Survived)
# why did we predict and predict based on predict? 
# ROCR does not understand predict, we need to translate it to prediction in ROCR

# calculate and compare performance
perf_tree<-performance(pred_val_tree,'tpr','fpr')  #tpr: true positive rate, my data says survive, model is correct, actual survice
#fpr: false positive rate, model is wrong
perf_logit <- performance(pred_val_logit,'tpr','fpr')
#create the 'lift and gains' chart
# diagonal- the pig, indifferent to tp or fp
# goal: 1. close to tp as possible. do not use models below the pig
      #2. greatest area under the curve(AUC)

plot(perf_tree,col="black")
plot(perf_logit,col='blue',add=TRUE) # plot on same chart
# initially both models perform well, low fpr, high tpr. then it gets tricky. perf_logit is better.
# low false positive rate is more important. 