rm(list=ls())

#########Mann Pawar###############

#Q1
Q1 <- read.csv("data/HW2.csv")

library(semTools)
library(tidyverse)

#Q2
Q1DF <- data.frame(Q1)
Q2 <- table(Q1DF$Form)
#Canned  Dried  Fresh Frozen  Juice 
#12      9     24      6     11

#Q3
Q3 <-  prop.table(Q2)
#Canned      Dried      Fresh     Frozen      Juice 
#0.19354839 0.14516129 0.38709677 0.09677419 0.17741935 

#Q4
Q4 <- var(Q1DF$RetailPrice) ; #4.277157

#Q5
Q5 <- sd(Q1DF$RetailPrice) ; #2.068129

#Q6
Q6 <- semTools::kurtosis(Q1DF$RetailPrice) #z value is 4.001

#Q7
Q7 <- "B" 

#Q8
Q8 <- diff(range(Q1DF$RetailPrice)) ; #10.1923

#Q9
Q9 <- IQR(Q1DF$RetailPrice) ; #2.36975

#Q10
ggplot(Q1DF, aes(RetailPrice)) + geom_histogram(binwidth = 1, color = "black", fill = "yellow")
Q10 <- semTools::skew(Q1DF$RetailPrice) ; Q10

#Q11
Q11 <- "B"
