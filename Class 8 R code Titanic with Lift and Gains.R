#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("titanic")
#install.packages("ROCR")
  ##############################
  #Creating a logistic regression for titanic
  ##############################
library(rpart)
mydf_train <- as.data.frame(titanic_train)
summary(mydf_train)#let's see what's in the data
View(mydf_train)
#creating a champion model with the titanic_train dataset:
my_logistic <- glm(Survived ~ Pclass + Sex + Age+ SibSp, data = titanic_train, family="binomial")
summary(my_logistic)
#predicting probability of 1
predict_logit <- predict(my_logistic, mydf_train, type="response")#
print(predict_logit)

  ##############################
  #Creating a tree for titanic
  ##############################
library(titanic)
mytree <- rpart(Survived ~ Pclass + Sex + Age+ SibSp, data = titanic_train, method = "class")#, control=rpart.control(minsplit=50, cp=0.013))
rpart.plot::rpart.plot(mytree, type = 1, extra=1, box.palette =c("pink", "green"), branch.lty=3, shadow.col = "gray")

plotcp(mytree)#getting the best cp value, do we need to go back and prune?
#use the cp value that has the lower error

############################################
#####The section below is optional##########
############################################
#The section below will explain the basics of
#model comparison and will code
#the lifts and gains chart
#### Create the lift and gains chart
#Scoring the model
library(ROCR)
mydf <- as.data.frame(titanic_train)
val_1 <- predict(mytree, mydf, type="prob")#We want to predict probability of 1 for each observations
print(val_1)
#Storing Model Performance Scores

pred_val <- prediction(val_1[,2], mydf$Survived)
pred_val_logit <- prediction(predict_logit, mydf$Survived) 
#we need performance
perf <- performance(pred_val,"tpr","fpr")
perf_logit <- performance(pred_val_logit,"tpr","fpr")
#Plotting Lift Curve
plot(perf,col="black",lty=3, lwd=3)
plot(perf_logit,col="blue",lty=3, lwd=3, add=TRUE)
#plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

