###################Mann Pawar#######################
rm(list=ls())

#Q1
Q1 <- 10+30*(1-0.15/25)^5
Q1 ##39.11074

#Q2
Q2 <- c(25,45,50,30,60)

#Q3
Q3 <- mean(Q2)
Q3 ##42

#Q4
Q4 <- max(Q2)
Q4 ##60

#Q5
Q5 <- sd(Q2)
Q5 ##14.40486

#Q6
Q6 <- seq(from = 0, to = 50, by = 10)
Q6

#Q7
x <- seq(from = 10, to = 90, by = 10)
Q7 <- matrix(x,3, 3, byrow = TRUE)
Q7

#Q8
income <- c(40000, 60000, 80000)
spending <- c(20000, 30000, 50000)

Q8 <- cbind(income, spending)
Q8 #     income spending
   #[1,]  40000    20000
   #[2,]  60000    30000
   #[3,]  80000    50000

#Q9
Q9 <- "observation"

#Q10
Q10 <- "<-"


