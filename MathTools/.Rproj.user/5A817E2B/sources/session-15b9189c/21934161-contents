rm(list=ls())
library(tidyverse)
#####Mann Pawar#######

#Q2
f <- function(x) 6*x^2 - 2*x + 4
h <- 0.0001
P2 <- (f(2 + h) - f(2))/h       #22.0006

#Q3
P3a <- 50/320 #0.15625
P3b <- 120/320 #0.375

#Q4
wait_times <- rexp(50,5)
wait_timesDF <- data.frame(wait_times)
wait_timesDF <- wait_timesDF %>% mutate(wait_times_sqrt=sqrt(wait_times))

ggplot(wait_timesDF, aes(wait_times)) + geom_histogram(bins=10)
ggplot(wait_timesDF, aes(wait_times_sqrt)) + geom_histogram(bins=10)
###The square root values look more normal as compared to the raw values

#Q5
X <- c(5, 7) ; Y <- c(9, 4)
BoundXY <- rbind(X, Y)
scale(BoundXY)

P5 <- dist(BoundXY, method="euclidean")  #5 is the euclidean distance between customers X and Y

#Q6

A <- matrix(c(2,1,1,1,3,2,3,4,-5), 3, 3, byrow=T)
B <- c(11, 19, -2)

P6 <- solve(A,B) #x=2, y=3, z=4

#Q7
P7a <- 13/20  #0.65 is the probability that a person does not subscribe
P7b <- 7/20   #0.35 is the probability that a person does subscribe
#The odds in favor of purchasing the subscription are 7:13

#Q8
quadruple_plus_three <- function(x) 4*x + 3
P8 <- quadruple_plus_three(5)    #23

#Q9
data(txhousing)
tx.cleaned <- txhousing %>% select(-city,-year,-month,-date)
tx.cleaned <- na.omit(tx.cleaned)
tx.scaled <- scale(tx.cleaned)

pca_tx<-prcomp(tx.scaled)
summary(pca_tx)

P9a <- 0.8083   #0.8083
P9b <- 3        #3 that is PC1, PC2, PC3

#Q10
P10 <- pbinom(4,40,0.12) #0.466910964758293

#Q11
P11a <- factorial(4)  #24 ways to arrange the 4 books on a shelf
P11b <- factorial(9)/factorial(6)  #504 ways to select 3 students out of 9 students 



