rm(list=ls())

#############Day 2 Math Tools: Probability##########################
#################MANN PAWAR############################

data <- read.csv("data/Universities.csv")
library(tidyverse)
####only include numeric variables in distance matrix










################Probability##########################

             #0 cards     #1  #2    #3   #4 or more
Probability <- c(.025, .098, .166, .165, .546)

#What is the probability that someone has 0 cards?
.025

#What is the probability that a reader carries less than 2 cards?
##P(X<2) = P(X=0) + P(X=1)

sum(Probability)

##What is the probability that a reader carries 2 or more cards?
##P(X>=2)
.166 + .165 + .546 ##.877
1-.123 #.877

##expected values, variance, and sd given prob of a discrete distribution

Bonus <- c(10,6,3,0)
Probability <- c(.15,.25,.4,.2)

sum(Probability)

weightedVector <- Bonus * Probability; weightedVector

weightedMean <- weighted.mean(Bonus, Probability); weightedMean #4.2

weightedVar <- (Bonus-weightedMean)^2*Probability
FinalVar <- sum(weightedVar)
#9.96 is variance 
sqrt(FinalVar)

defect <- read.csv("data/defects.csv")
summary(defect)

samplespace <- 0:7

frequency <- table(defect$NumDefects); frequency
proportions <- prop.table(frequency); proportions
cumulativeproportions <- cumsum(proportions) ; cumulativeproportions

Defects <- as.data.frame(t(rbind(samplespace, frequency, proportions, cumulativeproportions)))
Defects

#How many defects should the manufacturer expect per monitor (E(X))?
ExpValue <- sum(Defects$samplespace*Defects$proportions)
#3.714

##variance
deviations<-(Defects$samplespace - ExpValue)^2 *Defects$proportions
VarDefects <- sum(deviations)
VarDefects #4.876204
##sd
sqrt(VarDefects) #2.208213


##free throw example
n <- 6
p <- .894

ex <- n*p; ex #5.364
VarFreeThrows <- n*p*(1-p); VarFreeThrows #0.568584

sdFreeThrows <- sqrt(VarFreeThrows) ; sdFreeThrows #0.7540451

#####
########10% workers did not think they were going to retire
##we surveyed 10 people n=10, what is the probability that no one thinks they are going to retire?

##p = .1, n=10, X=0
##P(X=0) given .1 and sample size of 10
dbinom(0, 10, 0.1) ##0.3486784

##What if probability was 0.2 instead of 0.1
#given P(X=0) and size of 10
dbinom(0,10,0.2) #0.1073742

##<3 workers think they wont be able to retire
##same p at .2 and n at 10
##P(X<3) = P(X<=2) = P(X=0) + P(X=1) + P(X=2)
pbinom(2, 10, 0.2) #0.6777995

dbinom(0, 10, .2) + dbinom(1, 10, .2) + dbinom(2, 10, .2)
#0.6777995

##100 trials and a prob of success at .68
##70 as x given different questions

##P(X=70)
dbinom(70, 100, .68) #0.07907911

##P(X<=70)
pbinom(70, 100, .68) #0.7006736

##P(X<70) or P(X<=69)
pbinom(69,100,0.68) #0.6215945
0.7006736 - 0.07907911 #0.6215945

##P(X>70)
1-pbinom(70, 100, .68) #0.2993264
pbinom(70, 100, .68, lower.tail = F) #0.2993264

##P(X>=70) = P(X>69)
1 - pbinom(69,100,0.68)  #0.3784055
pbinom(69,100,0.68, lower.tail = F)  #0.3784055

##>70   +=70
0.2993264 + 0.07907911 #0.3784055

##free throws 5 out of 5 with .903
dbinom(5, 5, .903)

##free throws .805 P(X>=5) or P(X>4) out of 6 free throws
pbinom(4, 6, 0.805, lower.tail = F) #0.6676464

dbinom(5,6, 0.805) + dbinom(6,6, 0.805) #0.6676464

##credit card balances
##p=.35 n=6 P(X<2) = P(X<=1)
pbinom(1,6,.35) #0.3190799
dbinom(0,6,.35) + dbinom(1,6,.35) #0.3190799

dbinom(1, 10, 0.02)
dbinom(3, 20, 0.1)
dbinom(2,9,0.08)

pbinom(9, 10, 0.9)
pbinom(15, 20, 0.8)
#6a
dbinom(1, 12, 1/6)
#6b
pbinom(2, 12, 1/6, lower.tail = F)
#6c
pbinom(7, 12, 1/2, lower.tail = F)


#####Normal Distribution############

##standard normal (with mean=0 and sd=1)

#P(Z<=0) given standard normal
pnorm(0) #.5
pnorm(0, mean=0, sd=1) #.5

#P(Z>0)
pnorm(0, lower.tail=F) #.5 #Upper tail at Z>0 is 50%
##50% likelihood of being above 0 and also 50% likelihood being below 0

#P(Z<z) = .5
qnorm(.5) #0

##P(Z>z) = .5
qnorm(.5, lower.tail = F) #0

###z-score formula

##p(X>60) given mean of 72 and sd of 8

#(x - mean)/sd

(60-72)/8 #-1.5

#P(Z>-1.5)
pnorm(-1.5, lower.tail = F) #0.9331928

#P(X>60)
pnorm(60, 72, 8, lower.tail = F) #0.9331928

##laptop battery

#P(X>8) given mean of 6 and sd of .9
pnorm(8, 6, .9, lower.tail = F) #0.01313415

##P(X<16) given mean of 15 and sd of 3.5
pnorm(16, 15, 3.5) #0.6124515

##between 2 numbers, use f(b) - f(a) format, where b is the higher number
##P(-1.52<Z<1.96) = P(1.96) - P(-1.52)
pnorm(1.96) - pnorm(-1.52) 
#0.9107466

z = (1530 - 1250)/150 ; z

pnorm(1.95, 2, 0.05)


##P(X > x) = .1 === P(X<x) = .9
##given mean of 7.49 and sd of 6.41
qnorm(.1, 7.49, 6.41, lower.tail = F) #15.70475
qnorm(.9, 7.49, 6.41) #15.70475


##Stock price example 95% given mean of 58.5 and sd of 8.25
##P(X<x) = .95
qnorm(.95, 58.5, 8.25) #72.07004
qnorm(.05, 58.5, 8.25, lower.tail = F) #72.07004
##The 95th percentile of the price of the stock would be 72.07004

##teacher salary example top 2.5% given mean of 50000 and sd of 2500
##P(X>x) = .025
qnorm(.025, 50000, 2500, lower.tail = F) #54899.91
#Anyone with a salary above 54899.91 will not get a raise

##sleeping bag example mean of 32, sd of 8, P(X<x) = .05
qnorm(.05, 32, 8)
#18.84117 is the temperature where the sleeping bag will be too cold only 5% of the time.

qnorm(0.95, 500, 10)

##############Standard Error#################

##from (x - m)/sd to (x - m)/(sd/sqrt(n)) ---- is a sample size available?

##P(X<78) given a mean of 80 and an sd of 6
pnorm(78, 80, 6) #0.3694413

##P(X<78) but interested in 4 students
pnorm(78, 80, 6/sqrt(4))
#there is approximately a 25.2% chance that the average score of 4 students is below 78

##lingering campaign
P(Xbar>.46) given a mean of 4.18 and sd at .84 with sample size of 50

pnorm(4.26, 4.18, .84/sqrt(50), lower.tail = F) #0.2503353

##lowering sample size will increase what we found in regards to the percentage
pnorm(4.26, 4.18, .84/sqrt(2), lower.tail = F) #0.4464297


##Flu example
##P(25 < Xbar < 30) given a mean of 28 and sd of 8 with n = 36
#P(Xbar < 30) - P(Xbar < 25)

pnorm(30, 28, 9/sqrt(36)) - pnorm(25, 28, 9/sqrt(36)) 
#0.8860386


##labor example 
#P(Xbar > 20) given a mean of 22, sd=2, and an n=8
pnorm(20, 22, 2/sqrt(8), lower.tail=F)
#0.9976611

pnorm(20, 22, 2/sqrt(100), lower.tail=F)
#1

##Using proportions in normal distributions

##P(Phat > .46) given p = .43 and n=50
#women lingering marketing effect
pnorm(.46,.43, sqrt(.43*(1-.43)/50), lower.tail = F)
#0.3341494

#teenage girls
##P(Phat >= .34) given p = .21 and n=50
pnorm(.34,.21,sqrt(.21*(1-.21)/50), lower.tail = F)
#0.01200832

##Labor force example
p <- .637
phat1 <- .625
phat2 <- .6
n <- 120

pnorm(phat1, p, sqrt(p*(1-p)/n)) - pnorm(phat2, p, sqrt(p*(1-p)/n))
#0.192639

############Contingency Table Examples##############

# Either Instagram or Caucasian  (Joint Probability)
(170 + 290 - 120)/400 #0.85

x <- c(50, 60, 120, 170)
matrix_x <- matrix(x, 2, 2, byrow = T) ; matrix_x

total <- sum(x); total

rowsums <- margin.table(matrix_x, 1); rowsums
colsums <- margin.table(matrix_x, 2); colsums

prob <- prop.table(matrix_x) ; prop
rowprob <- margin.table(prob, 1) ; rowprob
colprob <- margin.table(prob, 2) ; colprob

#what is the prob that a randomly selected person prefers instagram or is caucasian
.425 + .725 - .3  #0.85


##what is the prob that a randomly selected person is latino and prefers pinterest (Intersection)
60/400   #0.15

##Conditional example
#Given a person is caucasian, what is the prob that they prefer Instagram
#P(Instagram|Caucasian)
120/290  #.41379


##What is the prob that a randomly selected person does not prefer pinterest? (Complement Rule)
1 - (230/400)

###
(24 + 72 + 44 + 88)/400

##P(E|O)
44/132

300/850 

0.05/0.95
20/500
0.96/0.04
96/4
99 * 500
49500 * 500



####################Transformations####################

dist <- read.csv("data/dist_facility.csv")
summary(dist)
library(tidyverse)

##VALUE

ggplot(dist, aes(VALUE)) + geom_histogram(fill="#7463AC", color="black") + labs(x="Miles to nearest substance abuse facility", y="Number of Counties")
#####extremely right skewed, extremely leptokurtic


dist.cleaned <- dist %>% 
  select(VALUE) %>% 
  mutate(miles.cube.root = VALUE^(1/3)) %>% 
  mutate(miles.log = log(VALUE)) %>% 
  mutate(miles.inv = 1/VALUE) %>% 
  mutate(miles.sqrt = sqrt(VALUE))

summary(dist.cleaned)

Cube <- ggplot(dist.cleaned, aes(miles.cube.root)) + geom_histogram(fill="darkblue", color="black") + labs(x="Cube Root") ; Cube

LogC <- ggplot(dist.cleaned, aes(miles.log)) + geom_histogram(fill="darkblue", color="black") + labs(x="Log") ; LogC

InverseC <- ggplot(dist.cleaned, aes(miles.inv)) + geom_histogram(fill="darkblue", color="black") + xlim(0,1) + labs(x="Inverse") ; InverseC

SqrC <- ggplot(dist.cleaned, aes(miles.sqrt)) + geom_histogram(fill="darkblue", color="black") + labs(x="Square root") ; SqrC

gridExtra::grid.arrange(Cube, LogC, InverseC, SqrC)

##Cube root looks like it normalized the data
summary(dist.cleaned$miles.cube.root)
mean(dist.cleaned$miles.cube.root) #2.662915
sd(dist.cleaned$miles.cube.root) #0.7923114

##P(X<3)
pnorm(3, 2.66, 0.7923)
#0.6660858


#Homework
dbinom(2, 20, 0.15)

pbinom(9, 10, 0.9)

120/400

2/9






