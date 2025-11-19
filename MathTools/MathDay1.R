rm(list=ls())

###########Day 1 Math Tools: Notation##############

##simple mean function

x <- 1:3; x

sample_mean <- sum(x)/length(x) ; sample_mean

###built in function
mean(x)

##User defined function
my_mean <- function(x){
  total <- sum(x)
  n <- length(x)
  result <- total/n
  return(result)
}

my_mean(x)

y <- 1:10
my_mean(y)

####Sample Variance Calculation####
###already have x defined as 1:3

sample_variance <- sum(((x - mean(x))^2)/(3-1))
sample_variance

###function in R
var(x)

###User Defined function
my_variance = function(x){
  n <- length(x)
  m <- mean(x)
  var <- sum((x-m)^2)/(n-1)
  return(var)  
}

my_variance(y)
my_variance(x)


###Dot Product

a <- c(2,4,6)
b <- c(1,3,5)

2*1 + 4*3 + 6*5
dot_product <- sum(a*b) ; dot_product

a %*% b

my_dot_product <- function(a,b){
  if(length(a)!=length(b)){
       stop("Vectors must be of equal length")
  }
  
  result <- sum(a*b)
  return(result)
}

c <- (1:2)
my_dot_product(a,c)

my_dot_product(a,b)


##Weighted mean

y <- c(.8, .9, 1, .75) ##students grades for each category
w <- c(.25, .4, .2, .15) ##weights of each category

sum(w) ##sum of weights equals one for 100%

sum(y*w) ##0.8725

weighted.mean(y,w) ##87.25%

my_weighted_mean = function(x, w) {
    if(length(x) != length(w)) stop("x and w must be the same length")
    if(sum(w) == 0) stop("Sum of weights cannot be zero")
    weight_mean <- sum(x*w)/sum(w)
    return(weight_mean)
  
}

my_weighted_mean(y, w)

my_weighted_mean(x, w)
w <- c(0, 0, 0, 0)

my_weighted_mean(y, w)


x <- c(2, 4, 6, 8, 9)

y <- c(1, 3, 4, 10, 8)

mean_x <- mean(x)
mean_y <- mean(y)

cov_xy <- sum((x-mean_x)*(y-mean_y))/(5-1) ###9.8 
cov(x,y)

cov_xy/(sd(x)*sd(y)) ##0.9246106
cor(x,y)  #0.9246106

my_correlation <- function(x,y){
  n <- length(x)
  mean_x <- mean(x)
  mean_y <- mean(y)
  numerator <- sum((x-mean_x)*(y-mean_y))/(n-1)
  denominator <- sd(x)*sd(y)
  r <- numerator/denominator
  return(r)
}

my_correlation(x, y) ##0.9246106


x <- c(3,5,8,3,20,23,4,1)
sum(x)/length(x)

A <- c(100, 40, 24, 86, 93)
B <- c(10, 35, 40, 10, 80)
cov(A, B)
my_correlation(A, B)

###MSE and MAE

actual <- c(3, 5, 2, 7)
predicted <- c(2, 6, 2.5, 8)

##MAE

MAE <- sum(abs(actual - predicted))/length(actual)
##0.875
MAE

my_mae <- function(actual, predicted){
  errors <- abs(actual - predicted)
  mae <- sum(errors)/length(actual)
  return(mae)
}

my_mae(actual, predicted)

##MSE
my_mse <- function(actual, predicted){
  errors <- actual-predicted
  mse <- sum(errors^2)/length(actual)
  return(mse)
}
my_mse(actual, predicted) #0.8125

my_mape <- function(actual, predicted){
  numerator_mape <- sum(abs((actual-predicted)/actual)) * 100
  denominator_mape <- length(actual)
  mape <- numerator_mape/denominator_mape
  return(mape)
}

my_mape(actual, predicted)

theta1 = 10 - 0.1*4
theta1

theta2 = 5.5 - 0.2*(-3)
theta2

sample(1:38, 7)

library(tidyverse)
opioid.policy <- read.csv("data/pdmp_2017.csv", stringsAsFactors = T)
nrow(opioid.policy)
summary(opioid.policy)

set.seed(502)
RNGkind(sample.kind="Rejection")
opioid.policy %>% select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>% 
  sample_n(size=25, replace=F) %>% summary()


#####Z score Calculation

x <- c(10, 12, 8, 9, 11)

x_mean <- mean(x)
x_sd <- sd(x)

z_score <- (x-x_mean)/x_sd ##standardizing each observation of x
z_score

scale(x)

set.seed(29)
x <- 1:20
sample(x, size = 4, replace=T)


####

Income <- c(30, 40)
Age <- c(25, 35)

df <- cbind(Income, Age)
df

df_scales <- scale(df); df_scales

dist(df_scales, method = "manhattan")

df <- data.frame(x=c(1,2), y=c(3,4))

dist(df, method = "euclidean")
dist(df, method = "manhattan")


TestScores <- c(72, 85, 90, 68, 75)
TS_mean <- mean(TestScores)
TS_sd <- sd(TestScores)

TS_Zvalue <- (TestScores - TS_mean)/TS_sd

scale(TestScores)

monthly_spending <- c(250, 300, 400, 275, 500, 325)
monthly_spending_mean <- mean(monthly_spending) ; monthly_spending_mean
monthly_spending_sd <- sd(monthly_spending) ; monthly_spending_sd

MS_Zvalue <- (monthly_spending - monthly_spending_mean)/monthly_spending_sd
MS_Zvalue

scale(monthly_spending)

x <- c(2,3)
y <- c(5,7)
distdf <- cbind(x,y)

dist(distdf, method = "manhattan")

Income <- c(40, 60)
Spending_Score <- c(20, 80)

CustDF <- cbind(Spending_Score, Income)
dist(CustDF)

XProdRate <- c(5,3)
YProdRate <- c(1,4)
ProdRateDF <- cbind(XProdRate,YProdRate)
dist(ProdRateDF)

######Simulation for CLT
library(tidyverse)

set.seed(42)
x <- rnorm(3000)
df <- data.frame(x)
ggplot(df, aes(x)) + geom_density()

d <- 1:6
mean(d) ##expected value is mean(d) is 3.5

NumberofRolls <- 300
set.seed(10)
x <- sample(d, NumberofRolls, replace = T)
mean(x)
df <- data.frame(x)
ggplot(df, aes(x)) + geom_density()

t <- 0
for(i in 1:1000){
     NumberofRolls <- 100
     x <- sample(d, NumberofRolls, replace=T)
     t[i] <- mean(x)
     }

df <- data.frame(t)
ggplot(df, aes(t)) + geom_density()


##second simulation exponential data in R

set.seed(10)
population <- 100000
right_tailed_sample <- rexp(population) - 1

mean(right_tailed_sample)
sd(right_tailed_sample)

data <- data.frame(right_tailed_sample)
ggplot(data, aes(right_tailed_sample)) + geom_density()

t <- c()
for(i in 1:300){
  r <- sample(data$right_tailed_sample, 30)
  t[i] = mean(r)
}
mean(t)
hist(t)

triple_plus_one <- function(x){
  new_x <- (3*x+1)
  return(new_x)
}

triple_plus_one(4)
