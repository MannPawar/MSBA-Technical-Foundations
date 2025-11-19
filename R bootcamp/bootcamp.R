rm(list=ls())   #Q1

######################Mann Pawar#########################

#Q2
sequence <- seq(from=0, to=70, by=7)

#Q3
answer <- 4 * (2 - (25/0.43))^2 + 3
#12609.5895078421

#Q4
library(tidyverse)
library(semTools)

#Q5
bootcamp <- read.csv("data/bootcamp.csv")

#Q6
bootcamp$region <- as.factor(bootcamp$region)
bootcamp$division <- as.factor(bootcamp$division)
bootcamp$metro <- as.factor(bootcamp$metro)
bootcamp$sales <- as.numeric(bootcamp$sales)
bootcamp$bonus <- as.numeric(bootcamp$metro)

summary(bootcamp)

#Q7
topSales <- bootcamp %>%  filter(sales > 1200000)
tail(topSales)

#Q8
ggplot(bootcamp, aes(division, sales, fill=division)) + geom_boxplot() + scale_fill_manual(values=c("gray", "red","darkblue","green","pink")) + theme_light() + labs(title="Sales by Divison Boxplot",x="Division",y="Sales")
##The visualization does not show any outliers.

#Q9
NoMissing <- na.omit(bootcamp)
nrow(NoMissing) #37

#Q10
qMean <- mean(bootcamp$sales) ; qMean #1058871
qStdev <- sd(bootcamp$sales) ; qStdev #111689
qMax <- max(bootcamp$sales) ; qMax #1299699
qMin <- min(bootcamp$sales) ; qMin #828677

#Q11
R2_data <- bootcamp %>% filter(region == "R2")
head(R2_data)

#Q12
bootcampDF <- data.frame(bootcamp)
frequencies <- table(bootcampDF$region)

#Q13
rewrite <- arrange(bootcamp, desc(bootcamp$bonus))


#Q14
ggplot(bootcamp, aes(sales)) + geom_histogram(binwidth=50000,color="yellow", fill="green") + labs(title="Skewness test histogram")
semTools::skew(bootcamp$sales) ##z=0.430
skewResults <- "A"

#Q15
ggplot(bootcamp, aes(region)) + geom_bar(show.legend = F,fill=c("green","purple","red","darkblue")) + theme_light() + labs(title = "The Region Barchart",x="Region",y="Number of observations")
##R1 and R3 have the highest number of observations. R4 has the least number of observations. 





