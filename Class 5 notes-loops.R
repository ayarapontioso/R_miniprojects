########
#for loop#
#for loop that prints i with print(i)
########
for (i in 1:4)  {
  print(i)
}

my_vect <- c()
for (i in 1:10) {
  my_vect[i] <- i
  # or my_vect <- append(my_vect,i)
}
print(my_vect)

# create a for loop taking value of purpose for each and put in a new column
for (i in 1:nrow(mydf)) {  # or (i in 1:length(mydf$purpose)), 
                           # nrow applies to dataframe, length applies to matrix
  mydf$new_col[i] <- mydf$purpose[i]
  return(mydf$new_col)
}
print(mydf)

#######
#conditional if statement#
pk <-c(1,2,3,4,5,6)

if (pk[2] == 2) { print("Yupii!")}
  #put conditions on the sum of elements:

#######
#Multi condition if statement#
if (pk[2]==4) {
  print("Yuppi!") 
} else {    # run if statement on the same line, if no run else after the last }
  print("Nay!")
}

#create a for loop that checks every observation in the g_b variable
# if the value is one (good), then mark the ob as "positive" in a new variab
# called description
for (i in 1:nrow(mydf)) {
  if (mydf$good_bad[i] == 1) {
    mydf$description[i] <- "positive"
  } else {
      mydf$description[i] <- 'negative'
    }
}
table(mydf$description)

#interpulate NA in purpose using value from previous observation
for (i in 1:nrow(mydf)) {
  if (is.na(mydf$purpose[i])) {
    mydf$purpose[i] <- mydf$purpose[i-1]
  }
}

for (i in 1:nrow(mydf)) {
  if (mydf$purpose[i]=="X") {
    mydf$purpose[i] <- mydf$purpose[i-1]
  }
}
summary(is.na(mydf$purpose))

#user defined function
#design a function that creates a sum of 3 elements xyz
my_func <-function(x,y,z) {
  my_sum <- z+y+x
  return(my_sum)
}
my_func(x=10,y=20,z=30)

#####
#prod facility 5 machines, make 3 mixes. 
mix1 <- c(1200,500,1050,860,720)
mix2<- c(1050,310,2100,990,880)
mix3 <- c(720,420,1700,1600,1120)

goal<- c(1000,480,1800,1000,990)
#weigh the mix123 so achieve as close to objective as possible
#user function with 3 inputs, returning: 
my_func <- function(m1,m2,m3) {
  goal<- mix1 * m1 + mix2 * m2 + mix3 * m3
  return(goal)
}

#linear optimization
install.packages('minpack.lm')
library(minpack.lm)
nlsLM(goal ~ my_func(m1,m2,m3),start=c(m1=0.1,m2=0.2,m3=0.3))
?nlsLM

#########
# Distributions - Exponential - describes time between events
# only one parameter: lambda-describes the rate of events arriving
# higher lambda = steeper curve

## time between each bart train
hist(sample(c(1,0), 800, replace =TRUE))
hist(rexp(50,rate=2),breaks=seq(from=0,to=100,by=2))
hist(rexp(50,rate=0.05),breaks=seq(from=0,to=100,by=2)) #rate of decr flatter
#business case: P(waittime x>5min) = e^(-lamda*5min)
#wait for bus longer than 5 min
exp(-2*5)
exp(-0.5*5)
#business case: P(waittime x<5min) = 1 - e^(-lamda*5min)
# wait for bus less than 5min
1-exp(-2*5)
1-exp(-0.5*5)