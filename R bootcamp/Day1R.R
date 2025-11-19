################################################################################
#Project Name: R bootcamp Day 1#################
#Code Author: Mann Pawar ############
#Date Last Edited: July 28, 2025################
#Data Used: ______________#################
#Libraries Used:______________##################


########## R handles Text multiple ways ################

'This is a string'
"This is also a string"

##R is a calculator"""""""""""""

3+4
3*4
3/4

3+4*100^2


2 + 3 * 5 - 7^2 %% 4 + (5/2)

2 + 3 * 5 - 49 %% 4 + (2.5)

#Making a dataset##################
income <- c(34000, 123000, 215000)
voted <- c("yes","no","no")

vote <- data.frame(income, voted)

##observations: people being measured
##variables: information about each person##

ncol(vote) ##2 variables or columns in this dataset

nrow(vote) ##3 observations or rows in this dataset

dim(vote) ##3 2 Shows the number of rows and number of columns in this dataset, dim stands for dimensions of the dataset

##making a few assignments##

kStates <- 29 ##numeric constant

A <- "Apple" ##string constant
A = "Apple" ##equivalent statement to above

print(A)
A

B <- 3 + 4 * 12 ; B ##semicolon is a good feature when you want to assign and print in one line

##########################Break#############################
#Whats allowed in naming objects########
AB <- "Allowed"
#123AB <- "Allowed" - Variable names cannot start with a number
#.AB <- "Allowed" - starting with a dot is not recommended format in naming objects
AB.1 <- "Allowed"
AB_1 <- "Allowed"


###Built In Function###########################3

sqrt(100)
max(100, 200, 300)
min(100, 200, 300)

x <- c(1,2,3,3,100,-10,40)

max(x)
min(x)
sort(x)

sqrt(x=100) #same as sqrt(100)

Sys.time()

seq(from=0, to=30, by=5)
seq(0,30,5) #same as the above

sort(x)
sort(x, decreasing=TRUE)

y <- 1:20 ; y
sort(y, decreasing = T) #TRUE and T is the same. FALSE and F is the same.

##we are installing Tidyverse
library(tidyverse)

##environment commands

ls() ##list objects in environment

rm(income, voted) ##removes objects in environment

#rm(list=ls()) #this function is useful at the top of each R script to clear your environment. Dont use this at the end or your script will not work because this will clear the environment at the end.

#######################Vectors############################
c(1, 2, 3, 4, 5)
1:5
seq(0, 50, 10)
rep(1:3, times=3)
rep(1:3, each=3)

names <- c("Sally","Shay","Sue")
length(names) ## 3

###############Matrices################
x <- 1:9 ##Making a matrix with this vector from 1-9

matrix(x, nrow = 3, ncol = 3) ##adding numbers down the col without byrow parameter changed

m1 <- matrix(x, nrow = 3, ncol = 3, byrow = TRUE)

dimnames(m1) <- list(c(2020,2021,2022), c("low", "medium", "high"))
m1

##vectors to use in a matrix
a <- c(1, 2, 3)
b <- c(4, 5, 6)

#combine as columns (column-bind)
matrix_cbind <- cbind(a,b)
matrix_cbind

#combine as row (row-bind)
matrix_rbind <- rbind(a,b)
matrix_rbind

##############DataFrames#################
state <- c("Alaska", "Arizona", "Arkansas")
year.legal <- c(1998, 2010, 2016)
ounce.lim <- c(1,2.5,3)

####make dataframe using data.frame function########
pot.legal <- data.frame(state, year.legal, ounce.lim)
pot.legal

rm(state,year.legal,ounce.lim)

ls()

##accessing datasets that are part of Base R

data("mtcars")
summary(mtcars)
str(mtcars)

##accessing a dataset from a package, means having the package pre-installed and library active
##Installed MASS via tools > install packages > MASS
library(MASS)

data("Insurance")
summary(Insurance)
head(Insurance, n=10) ##first 10 observations
tail(Insurance) ##last 6 observations


##Access variables using $ dollar sign symbol between dataset$variable
head(Insurance$Claims) 
summary(Insurance$Claims) 
sd(Insurance$Claims) #71.1624
var(Insurance$Claims) #5064.087
mean(Insurance$Claims) #49.23438


####Load Data from dataset from a data folder using relative directory

gss.2016 <- read.csv("data/gss2016.csv")

data("iris")
head(iris)
summary(iris)

speed <- read.csv("data/day1.csv")
##tidyverse has to be loaded with the library command before using read_csv
library(tidyverse)
gss.2016b <- read_csv("data/gss2016.csv")
