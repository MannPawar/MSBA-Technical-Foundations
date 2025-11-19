rm(list=ls())

####################################################
##Day 2: Descriptive Statistics#########
##Mann Pawar##################
##7/29/2025##################


#################Categorical Variable########################

#Frequency Table
sample_vector <- c("A","B","A","C","A","B","A","C","A","B")

data <- data.frame(sample_vector)

table(data$sample_vector)

frequencies <- table(data$sample_vector) ; frequencies
#A B C 
#5 3 2 


##calculate proportions
proportions <- prop.table(frequencies)
#A   B   C 
#0.5 0.3 0.2 

##calculate cumulative sums
cumulfreq <- cumsum(frequencies) ; cumulfreq
##calculate cumulative proportions
cumulprop <- cumsum(proportions) ; cumulprop

##rbind - to bind the rows together
frequency_table <- rbind(frequencies, proportions, cumulfreq, cumulprop) ; frequency_table

##transpose the table - use the t() function
frequency_table <- t(frequency_table) ; frequency_table

#Transform this into a data set
frequency_table <- as.data.frame(frequency_table)
frequency_table

Rating <- c("ML","AI","DB","AI","ML","ML","DB","ML","ML","AI","DB")
data <- data.frame(Rating)

table(data$Rating)

frequencies_Rating <- table(data$Rating)
proportions_Rating <- prop.table(frequencies_Rating)
cumulfreq_Rating <- cumsum(frequencies_Rating)
cumulprop_Rating <- cumsum(proportions_Rating)
frequency_table_Rating <- rbind(frequencies_Rating, proportions_Rating, cumulfreq_Rating, cumulprop_Rating) ; frequency_table_Rating

frequency_table_Rating <- t(frequency_table_Rating)

frequency_table_Rating <- as.data.frame(frequency_table_Rating)

frequency_table_Rating




#########################Quantitative Data###########################

##Central Tendency###############3

salaries <- c(40000, 40000, 65000, 90000, 145000, 150000, 550000)

mean(salaries) #154285.7

salaries2 <- c(40000, 40000, 65000, 90000, 145000, 150000, 550000, NA, NA)
mean(salaries2, na.rm = T)#154285.7

##all the summary statistics that are part of base R have the na.rm parameter
##mean,median,sd,var,sum,etc.

##median
median(salaries) #90000

GrowthFund <- c(-38.32, 1.71, 3.17, 5.99, 12.56, 13.47, 16.89, 16.96, 32.16, 36.29)


(12.56 + 13.47)/2 #13.015
median(GrowthFund) #13.015

mean(GrowthFund) #10.088

##mode -- the most frequently occuring observation
sort(table(salaries), decreasing = T)[1:3]
##40000 occurred 2 times so its the mode

sort(table(GrowthFund), decreasing=T)[1:5]
##all observations have same frequency so no mode here

day2csv <- read.csv("data/day2.csv")
mean(day2csv$CustomerValue)
median(day2csv$CustomerValue)
sort(table(day2csv$CustomerValue), decreasing=T) [1:5]

mean(day2csv$DeliveryTime)
median(day2csv$DeliveryTime)
sort(table(day2csv$DeliveryTime), decreasing=T) [1:5]

###Spread##############################################
library(semTools)
library(e1071)

e1071::skewness(salaries) ##########1.415062 since its positive, positive skewness could be present

semTools::skew(salaries)
#skew (g1)        se         z         p 
#     2.311     0.926     2.496     0.013 
##z score is 2.496 which is a standardized measure of skewness.
##positive z score points to a right skew.
##this is skewed as z score > 2 here and our observations are less than 50.

semTools::skew(GrowthFund)
##sample size was 10
##z value is -1.783
##therefore this vector is normal according to this test

# Making an appropriate data.frame to use the hist() command
HousePrice <- c(430, 520, 460, 475,
                670, 521, 670, 417, 533, 525, 538,
                370, 530, 525, 430, 330, 575, 555,
                521, 350, 399, 560, 440, 425,
                669,660, 702, 540, 460, 588, 445,
                412, 735, 537, 630, 430)
HP <- data.frame(HousePrice)
###ggplot2, which is under tidyverse, requires data to be in a dataframe before formatting a chart

hist(HP$HousePrice)
nrow(HP) #36 observations so considered small

##adding some properties
hist(HP$HousePrice, breaks=5, main="A Histogram", xlab="House Prices (in $1,000s)", col="yellow")

library(tidyverse)

ggplot(HP, aes(HousePrice)) + geom_histogram(binwidth = 100, boundary = 300, color = "black", fill = "yellow") + labs(title="A Histogram", x = "House Prices(in 1,000s)", y="Frequency")
summary(HP$HousePrice)



########################Sd and Var#########################

x <- 1:5 ; x
sum((x-mean(x))^2)/4

var(x) ##var function applies bessel correction (divides by n-1 instead of n)

sqrt(2.5)
sd(x)

var(HP$HousePrice) #10527.97
sd(HP$HousePrice)
summary(HP$HousePrice)


customers <- read.csv("data/customers.csv")

summary(customers)
nrow(customers) #200 observations medium sized dataset
str(customers)

summary(customers$Spending)


mean(customers$Spending) #659.555
median(customers$Spending) #662

sd(customers$Spending) #350.2876
var(customers$Spending) #122701.4

ggplot(customers, aes(Spending)) + geom_histogram(binwidth = 200, fill = "pink", color="black")

##skewness and kurtosis threshold +- 3.29
skew(customers$Spending)
##z value is -0.106 here so not skewed.

semTools::kurtosis(customers$Spending)
##z value is -3.425 which is less than -3.29 so it is platikurtic and kurtosis is present.

semTools::kurtosis(HP$HousePrice)
##mesokurtic: no kurtosis present as z value is -0.661.

attach(customers)

##Income skew and kurtosis and graph it
semTools::kurtosis(Income) ##2.978 mesokurtic
semTools::skew(Income) ##5.047 right skewed
ggplot(customers, aes(Income)) + geom_histogram(binwidth = 10000, color="black", fill="pink")

##HH Size
semTools::kurtosis(HHSize) #-3.725 platykurtic
semTools::skew(HHSize) #-0.513 no skewness
ggplot(customers, aes(HHSize)) + geom_histogram(color="black", fill="pink")

##Orders
semTools::kurtosis(Orders) #-0.201 mesokurtic
semTools::skew(Orders)  #4.553 right skewed
ggplot(customers, aes(Orders)) + geom_histogram(color="black", fill="pink")

##spending
summary(customers$Spending)
1250-50 ##range 1200  
962.2 - 383.8 ##IQR 578.4  
  
diff(range(customers$Spending))  
IQR(customers$Spending)  

##IQR stands for InterQuartile Range Q3 - Q1

##Spread Worksheet
sd(day2csv$CustomerValue) #21.82331
var(day2csv$CustomerValue) #476.2569
range(day2csv$CustomerValue) #40 - 150
IQR(day2csv$CustomerValue) #35

sd(day2csv$DeliveryTime) #30.01519
var(day2csv$DeliveryTime) #900.9119
range(day2csv$DeliveryTime) #41.42 - 273.88
IQR(day2csv$DeliveryTime) #24.03

##Skew and Kurtosis Worksheet

semTools::skew(day2csv$CustomerValue) #z value is 1.350
semTools::kurtosis(day2csv$CustomerValue) #z value is -1.055

semTools::skew(day2csv$DeliveryTime) #z value is 13.728
semTools::kurtosis(day2csv$DeliveryTime) #z value is 29.202

##Histogram Worksheet

ggplot(day2csv, aes(CustomerValue)) + geom_histogram(binwidth=15,color="yellow", fill="green") + labs(title="Green Histogram")

ggplot(day2csv, aes(DeliveryTime)) + geom_histogram(binwidth=25,color="yellow", fill="green") + labs(title="Green Histogram 2")

#################Change text data to numerical##################
evaluate <- c("excellent","good","fair","poor","excellent","good")
##change to numbers - excellent = 4 and so on.
evalNum <- ifelse(evaluate=="excellent", 4, ifelse(evaluate=="good", 3, ifelse(evaluate=="fair", 2, 1)))
evalNum

evalDF <- data.frame(evaluate)
evalDF <- evalDF %>% 
  mutate(evaluate = recode(evaluate,"excellent" = 4, "good" = 3, "fair" = 2, "poor" = 1))
evalDF

##Use stringsAsFactors to turn all text variables to categorical
customers <- read.csv("data/customers.csv", stringsAsFactors = TRUE)
summary(customers)

##Numeric Constant

kRhode <- 2.5
class(kRhode)

##Integer -- whole number - use as.integer
kTestInteger <- as.integer(4)


class(kRhode * kTestInteger)

##String 
kFirstName <- "Mann"
kLastName <- "Pawar"

Q20 <- "A"
kZipCode <- "23185"
class(kZipCode)

##logical test
kSixEight <- 6 > 8
class(kSixEight)

##
gss.2016 <- read.csv("data/gss2016.csv")
summary(gss.2016)

gss.2016$grass <- as.factor(gss.2016$grass)
class(gss.2016$grass)

##age variable

gss.2016 <- gss.2016 %>% mutate(age = recode(age, "89 OR OLDER" = "89"))
gss.2016$age <- as.numeric(gss.2016$age)
summary(gss.2016)

gss.2016 <- gss.2016 %>% mutate(age.cat = cut(age, breaks = c(-Inf, 29, 59, 74, Inf), labels=c("<30","30-59","60-74","75+")))

summary(gss.2016)


