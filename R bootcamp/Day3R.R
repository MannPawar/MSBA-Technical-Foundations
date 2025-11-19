rm(list=ls())

######Data Prep################

library(tidyverse)
evaluate <- c("excellent","good","fair","poor","excellent","good")
##change to numbers - excellent = 4 and so on.
evalNum <- ifelse(evaluate=="excellent", 4, ifelse(evaluate=="good", 3, ifelse(evaluate=="fair", 2, 1)))
evalNum

evalDF <- data.frame(evaluate)
evalDF2 <- evalDF %>% 
     mutate(evaluate = recode(evaluate,"excellent" = 4, "good" = 3, "fair" = 2, "poor" = 1))
evalDF

dataCaseWhen <- evalDF %>% 
     mutate(evaluate = case_when(evaluate == "excellent" ~ 4,
                                 evaluate == "good" ~ 3,
                                 evaluate == "fair" ~ 2,
                                 evaluate == "poor" ~ 1,
                                 TRUE ~ NA_real_)) ##safety check for unexpected values
dataCaseWhen            


score <- c(9, 6, 3, 8, 5, 10, 2)

category <- case_when(

     score > 8 ~ "High",
     ##score >= 5 ~ "Medium",
     score >= 5 & score <- 8 ~ "Medium",  
     score < 5 ~ "Low"
     )
category

df <- data.frame(score, category)
df

###Dates

strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates,"%m/%d/%Y")
str(dates)


stryyyymm <- c("202201", "202003", "202204")

library(lubridate)
datesym <- ym(stryyyymm)
str(datesym)

datesmdy <- mdy(strDates)
str(datesmdy)

UseDates <- read.csv("data/Lubridate.csv")
summary(UseDates)

UseDates$Date <- ym(UseDates$Date)

#########year and month function##########
UseDates$month <- month(UseDates$Date)
UseDates$year <- year(UseDates$Date)

UseDates1 <- UseDates %>% 
  mutate(season = case_when(
    month >= 3 & month <= 5 ~ "Spring",
    month >= 6 & month <= 8 ~ "Summer",
    month >= 9 & month <= 11 ~ "Fall",
    TRUE ~ "Winter"
  ))
summary(UseDates1)

UseDates1$season <- as.factor(UseDates1$season)
summary(UseDates1)

UseDates2 <- UseDates %>% 
  mutate(season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    TRUE ~ "Winter"
  ))

UseDates2 


######filter to show only year 2022 data
UseData2022 <- filter(UseDates1, year == 2022)
UseData2022

houseprices <- read.csv("data/day3.csv")
hpdatesym <- ym(houseprices$BuildDay)
str(hpdatesym)

houseprices <- houseprices %>% 
  mutate(HouseSizeCategory = case_when(
    Sqft < 2000 ~ "Small"
    Sqft %in% 2000:3000 ~ "Medium",
    Sqft > 3000 ~ "Large"
  ))
houseprices$HouseSizeCategory <- as.factor(houseprices$HouseSizeCategory)
table(houseprices$HouseSizeCategory)


######################################

gig <- read.csv("data/gig.csv", stringsAsFactors = T, na.strings = "")
summary(gig)
str(gig)

######Arrange by wage

sortwage <- arrange(gig, Wage)
head(sortwage, n=10)

####Arrange decreasing by Wage
sortWageDesc <- arrange(gig, desc(Wage))
head(sortWageDesc)



########customers.csv

customers = read.csv("data/customers.csv", stringsAsFactors = T)
summary(customers)

##Subsetting with [] we use [row,column] format

customers[1,] ##first record
customers[,1] ##first column
customers[1,2] ##first row, second column
customers[1:3,] ##first three records
customers[,1:3] ##first three columns


noFirstCol <- customers[,-1] # - sign excludes, here it excludes the first column (customerid)

SmallDataset <- customers[-c(1,4,5),] # removed 3 records to have a new number of rows at 197
nrow(SmallDataset)

###Using Filter Function
college <- filter(customers, College=="Yes")
nrow(college)


twoFilters <- filter(customers, College == "Yes" & Income < 50000)
nrow(twoFilters) ##21 observations where college == yes and income less than 50000

twoFiltersWOrStatement <- filter(customers, Race=="Black" | Race=="White")
nrow(twoFiltersWOrStatement) #136 observations where customers were either black or white

####select function

smallDataset <- select(customers, Income, Spending, Orders)
summary(smallDataset)

##Add Pipe Operator
smallDataset <- customers %>%  select(Income, Spending, Orders)
head(smallDataset)

##gig dataset -- counting
length(gig$Industry)
summary(gig)
nrow(gig)

gig %>% filter(Industry == "Automotive") %>% 
  count(Industry)

gig %>% filter(Industry == "Automotive") %>% 
  count()

filterResult <- gig %>% filter(Industry == "Automotive") %>% 
  nrow()

###Count how many Wage > 30
gig %>% filter(Wage>30) %>% nrow() ##536 Wages greater than 30 dollars

##Count how many Jobs that are accountants
gig %>%  filter(Job == "Accountant") %>% nrow() ##83 Jobs are Accountants


#############Missing Data################
##na.rm

y <- c(1,2,NA,3,4,NA)
sum(y, na.rm=T)

summary(gig)

table(gig$Job)

y <- na.omit(y)
y

sum(y)
mean(y)

sum(is.na(gig$Industry)) #10 cells with NA values in Industry

which(is.na(gig$Industry)) #which cells have NA values
#24 139 361 378 441 446 479 500 531 565

showBlankObs <- gig %>% 
  filter(is.na(Industry))

showBlankObs <- filter(gig, is.na(Industry))

sum(is.na(gig$Wage))

TurnNA <- gig %>% 
  mutate(Job=na_if(Job, "Other"))
head(TurnNA)

fiveormorebeds <- houseprices %>% filter(Beds>=5)
houseprices %>% filter(Beds>=5) %>%  nrow()

arrange(houseprices, desc(Price))

################################
library(Amelia)
data("africa")
summary(africa)

summary(africa$trade)

africa1 <- na.omit(africa)
nrow(africa) #120 obs
nrow(africa1) #115 obs


africa2 <- africa %>% drop_na()
nrow(africa2)

sum(is.na(africa)) ##7

nrow(africa) - nrow(africa1)  ##5 obs
which(is.na(africa))

data("airquality")
sum(is.na(airquality))
nrow(airquality)
airqual <- na.omit(airquality)
nrow(airqual)

mean(airquality$Solar.R, na.rm=T)
mean(airqual$Solar.R)

##########################################################

################Summarize or Summarise######################

gig %>% drop_na() %>% 
  summarise(mean.Wage = mean(Wage), 
            sd.Wage = sd(Wage), 
            var.Wage=var(Wage),
            iqr.Wage = IQR(Wage)
            )

semTools::skew(gig$Wage)
#n=604 ----------- 604 > 300 so considered large
## to see if problematically skewed, we use the threshold for large datasets +- 7

ggplot(gig, aes(Wage)) + geom_histogram(bins=8, color="salmon", fill="brown")

groupedData <- gig %>%  
  group_by(Industry) %>% 
  summarize(meanWage = mean(Wage))
groupedData

tapply(gig$Wage, gig$Industry, mean)

groupedData <- gig %>%  
  drop_na() %>% 
  group_by(Industry) %>% 
  summarize(meanWage = mean(Wage))
groupedData

##Mutate -- generic function to make a change to the dataset
africa.mutated <- africa1 %>% 
  mutate(calculation = gdp_pc*infl)
head(africa.mutated)


data("iris")
summary(iris)

iris1 <- select(iris, Sepal.Length, Petal.Length)
head(iris1)

Species_setosa <- filter(iris, Species == "setosa")
head(Species_setosa)

Sepal.Length.Arranged = arrange(iris,Sepal.Length)
head(Sepal.Length.Arranged)

iris.mutated <- iris %>% 
  mutate(log_petal.width = log(Petal.Width))
head(iris.mutated)

fifthquestion <- houseprices %>% 
  group_by(HouseSizeCategory) %>% 
  summarize(avg_price = mean(Price), avg_sqft = mean(Sqft))
table(fifthquestion)

#####################################################
gss.2016 <- read.csv("data/gss2016.csv")
summary(gss.2016)

gss.2016.cleaned <- gss.2016 %>% 
  ###########GRASS VARIABLE CHANGES#######
  mutate(grass=as.factor(grass)) %>% 
  mutate(grass=na_if(grass, "DK")) %>% 
  mutate(grass=na_if(grass, "IAP")) %>% 
  mutate(grass=droplevels(grass)) %>% 
  ################AGE VARIABLE CHANGES############)
  mutate(age = recode(age, "89 OR OLDER" = "89")) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age.cat = as.factor(case_when(
    age < 30 ~ "<30",
    age >= 30 & age <= 59 ~ "30-59",
    age >= 60 & age <= 74 ~ "60-74",
    age >= 75 ~ "75+",
    TRUE ~ NA_character_
  )))
  ############<30, 30-59, 60-74, 75+#################
summary(gss.2016.cleaned)



#######################################################
brfss <- read.csv("data/brfss.csv")
summary(brfss)
####Our variable of interest is PHYSHLTH
brfss.cleaned <- brfss %>% 
  select(PHYSHLTH) %>% 
  ##handle 77 and 99 by turning to NA
  mutate(PHYSHLTH=na_if(PHYSHLTH, 77)) %>% 
  mutate(PHYSHLTH=na_if(PHYSHLTH, 99)) %>% 
  drop_na() %>% 
  #####value 88 is actually 0 days of illness#######
  mutate(PHYSHLTH = recode(PHYSHLTH, "88" = 0L))


summary(brfss.cleaned)

#Central Tendency
mean(brfss.cleaned$PHYSHLTH, na.rm=T) #4.224106
median(brfss.cleaned$PHYSHLTH, na.rm=T) #0
sort(table(brfss.cleaned$PHYSHLTH), decreasing = T) [1:5]

#Spread with regard to the mean
var(brfss.cleaned$PHYSHLTH, na.rm = T) #77.00419
sd(brfss.cleaned$PHYSHLTH, na.rm = T) #8.775203

#Spread with regard to the median
diff(range(brfss.cleaned$PHYSHLTH, na.rm = T)) #30
IQR(brfss.cleaned$PHYSHLTH, na.rm = T) #3

ggplot(brfss.cleaned, aes(PHYSHLTH)) + geom_histogram(fill = "salmon", color="brown")

library(semTools)
skew(brfss.cleaned$PHYSHLTH) #z = 607.905 right skewed
kurtosis(brfss.cleaned$PHYSHLTH) #z = 478.063 leptokurtic


brfss.cleaned %>% 
  summarize(mean.days = mean(PHYSHLTH), sd.days = sd(PHYSHLTH), var.days = var(PHYSHLTH), median.days = median(PHYSHLTH), iqr.days = IQR(PHYSHLTH)) 
