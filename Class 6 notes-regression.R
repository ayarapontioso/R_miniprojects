library(readxl)
mydf<- read_excel("german credit card.xls")

#rub data, replace missing values X
mydf$purpose_num<-as.numeric(replace(mydf$purpose, "", "X"))[1:1000]

#rub data, transform good_bad to 1&0 and numeric
mydf$good_bad<-gsub(mydf$good_bad,pattern='good',replacement=1)
mydf$good_bad<-gsub(mydf$good_bad,pattern='bad',replacement=0)

mydf$good_bad<-as.numeric(mydf$good_bad)

#subset good bad
mydf_good <- mydf[which(mydf$good_bad == 1),c('checking','duration')]
mydf_bad <- mydf[which(mydf$good_bad == 0),1:2]

for (i in 1:ncol(mydf)) {
  mydf <- as.data.frame(mydf)
  hist(as.numeric(mydf[,i]))
}


##################
#causation between size of company and whether they sign up for Sales force?
size <- c(31,25,62,85,64,81,89,102)
signedup <- c(0,0,1,1,0,1,1,1)

my_sales <- as.data.frame(matrix(nrow=8,ncol=1))
my_sales$size <- size
my_sales$signedup <- signedup
my_sales

sales_mod <- glm(signedup ~ size, data=my_sales, family = 'binomial')
summary(sales_mod)

#p is large bc sample group too small
exp(0.1912) #how the odds of success changes

#percentage of change = (exp(b1)-1)*100%
(exp(0.1912)-1)
#this is how to interpret the coefficient
#by every unit of increase of coefficent, odds of success increase by x percent

#transformation from odds to percentage
##logit <- inter +beta1*x
##odds <-exp(logit)
##prob <- odds/(1+odds)

oddstopct <- function(inter, beta1, size)  {
  logit <- inter +beta1*size
  odds <-exp(logit)
  prob <- odds/(1+odds)
  return(c(odds,prob))
}

oddstopct(inter=-11,beta1=0.1912,size=75)
#increase size by 1 employee, odds of success goes up by 21%
oddstopct(inter=-11,beta1=0.1912,size=76)
#high success chance, 97%, sell!
oddstopct(inter=-11,beta1=0.1912,size=40)
#low success chance, 0.03, waste of time!

#########
#single variable logistic regression for german credit dataset
credit_mod <- glm(mydf$good_bad ~ age, data=mydf, family = 'binomial')
summary(credit_mod)

#age is significant
exp(0.01844)-1 
#for every age increase, odds of being good creditor increase by 1.8%

#prob for 45 yr old person has good credit?
age2cred <- function(inter, beta1, age)  {
  logit <- inter +beta1*age
  odds <-exp(logit)
  prob <- odds/(1+odds)
  return(c(odds,prob))
}
age2cred(inter=0.200919,beta1=0.018440,age=45)
age2cred(inter=0.200919,beta1=0.018440,age=50)

#########
#multivariate logistic regression
ger_mod_better <- glm(good_bad~age+duration+amount+savings+property,data=mydf,family='binomial')
summary(ger_mod_better)
#amount insignificant(no asteric), exclude it
#unless: there is a huge biz opportunity, but insignificant only to a certain degreee, max .7
ger_mod_better <- glm(good_bad~age+duration+savings+property,data=mydf,family='binomial')
summary(ger_mod_better)