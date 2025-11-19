rm(list=ls())

#########Data Visualization############################
##working with ggplot2###############

library(tidyverse)

GoUp <- .54385
GoDown <- .03809
RemainStable <- .34285
NoOpinion <- .07619

data_frame <- data.frame(
  Category = c("Go Up", "Go Down", "Remain Stable", "No Opinion"),
  Probabilities = c(GoUp, GoDown, RemainStable, NoOpinion)
)
data_frame

ggplot(data_frame, aes(Category, Probabilities, fill=Category)) + geom_bar(stat = "identity") + labs(title="How do you expect R's Market Share to Change", x="Opinion Category", y="Probability") + theme_minimal() + geom_text(aes(label = Probabilities), vjust=-.5, size = 4)

nhanes <- read.csv("data/nhanes2012.csv")
nhanes.cleaned <- nhanes %>% 
  select(AUQ300) %>% 
  mutate(AUQ300 = recode_factor(AUQ300,
                                "1" = "Yes",
                                "2" = "No")) %>% 
           drop_na(AUQ300)

summary(nhanes.cleaned$AUQ300)

ggplot(nhanes.cleaned, aes(AUQ300, fill=AUQ300)) + geom_bar() + labs(x="Gun Use", y="Number of Participants")

nhanes.cleaned %>% 
ggplot(aes(AUQ300, fill=AUQ300)) + geom_bar(show.legend = F) + labs(x="Gun Use", y="Number of Participants")

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

gss.2016.cleaned %>% 
  drop_na() %>% 
  ggplot(aes(grass)) + geom_bar(fill=c("red","purple")) + labs(x = "Should marijuana be legal?", y="Responses")                  

#####edit to include age.cat#################
gss.2016.cleaned %>% 
  drop_na() %>% 
  ggplot(aes(age.cat, y=100*(after_stat(count))/sum(after_stat(count)), fill=grass)) + geom_bar() + labs(x = "Should marijuana be legal?", y="Responses") + theme_classic() 

#Number 2

data("diamonds")

summary(diamonds)

diamonds %>% 
  drop_na() %>% 
  ggplot(aes(cut)) + geom_bar(fill=c("brown", "blue", "green", "pink","grey")) + theme_minimal() + labs(x = "Cut", y = "No. of Diamonds")

fbi <- read.csv("data/fbi_deaths.csv")
summary(fbi)
##use 3-7 to do a pie chart

fbi.small <- fbi[3:7,]
fbi.small <- fbi.small %>% 
  rename(Weapon = X)
summary(fbi.small)

ggplot(fbi.small, aes(x="", y=X2016, fill=Weapon)) + geom_col() + coord_polar("y") + theme_void()


####################Continuous Variable######################
############density plot####################

###########simulating dataset to use in the density
set.seed(42)
x <- rnorm(1000, 10, 2)
mean(x)

df <- data.frame(x)

##Base Density with dataset and numerical variable
ggplot(df, aes(x)) + geom_density()

#adding parameters
ggplot(df, aes(x)) + geom_density(color="darkblue", fill="lightblue", lwd=1, alpha=.5) ##lwd is line width and alpha is for transparency

##adding more parameters
ggplot(df, aes(x)) + geom_density(color = "#2A4533", fill="#8EAB97", alpha=.8) +
  geom_vline(aes(xintercept = mean(x)), color="red", linetype="dashed", lwd=.5)
  ##adding hexadecimal using "#______"

ggplot(diamonds, aes(carat)) + geom_density(color = "black", fill="gold")

semTools::skew(diamonds$carat)
semTools::kurtosis(diamonds$carat)

set.seed(72)
x <- rnorm(500, 80, 15)
mean(x)
df <- data.frame(x)

ggplot(df, aes(x)) + geom_density(color = "violet", fill="darkblue", alpha=.3) +
  geom_vline(aes(xintercept = mean(x)), color="salmon", linetype="solid", lwd=.2)

###############boxplot###############

GrowthFund <- c(-38.32, 1.71, 3.17, 5.99, 12.56, 13.47, 16.89, 16.96, 32.16, 36.29)
GF <- data.frame(GrowthFund)

summary(GF)
QuantData <- quantile(GF$GrowthFund)
QuantData

QuantData[3] #you can index positions using 1 through 5 inside [] and 3 is the 50% aka the median

16.9425 - 3.8750  #13.0675 IQR
IQRvalue <- IQR(GF$GrowthFund)

OutlierValue <- IQRvalue * 1.5 #19.60125
OutlierValue

#      0%      25%      50%      75%     100% 
# -38.3200   3.8750  13.0150  16.9425  36.2900 

3.8750 - (-38.3200) #42.195 25% - 0%

42.195 > OutlierValue
##True means there is an outlier to the left

36.2900 - 16.9425 #19.3475 100% - 75%

19.3475 > OutlierValue
##False means there is no outlier to the right

##lowerbound = Q1 - 1.5*IQR
##upperbound = Q3 + 1.5*IQR
3.8750 - OutlierValue #-15.72625 Lower Bound
16.9425 + OutlierValue #36.54375 Upper Bound

#-38.32, [Lower Bound -15.72625] 1.71, 3.17, 5.99, 12.56, 13.47, 16.89, 16.96, 32.16, 36.29, [Upper Bound 16.9425]

ggplot(GF, aes(GrowthFund)) + geom_boxplot(fill = "#9B8EAB")

data("mtcars")
summary(mtcars$mpg)

ggplot(mtcars, aes(mpg)) + geom_boxplot(color="#AAAAAA", fill="#AAA3F3")

QuantData <- quantile(mtcars$mpg)
QuantData
#  0%    25%    50%    75%   100% 
#10.400 15.425 19.200 22.800 33.900 

##lower bound Q1 - 1.5 * IQR
##upper bound Q3 + 1.5 * IQR

IQRvalue <- IQR(mtcars$mpg); IQRvalue
1.5 * IQRvalue #11.0625
15.425 - 11.0625 ##4.3625 LB
22.800 + 11.0625 ##33.8625 UB

sort(mtcars$mpg)
#[LB 4.3625] 10.4 10.4 13.3 14.3 14.7 15.0 15.2 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.2 19.7 21.0 21.0 21.4 21.4 21.5 22.8 22.8 24.4 26.0 27.3 30.4 30.4 32.4 [UB 33.8625] 33.9

semTools::skew(mtcars$mpg) ##not skewed
nrow(mtcars) ##Small dataset
semTools::kurtosis(mtcars$mpg) ##mesokurtic or normal
ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth=5, color="black", fill="pink")


ggplot(diamonds, aes(carat)) + geom_boxplot(color="darkred", fill="green")

scores <- c(55, 56, 58, 60, 62, 65, 66, 67, 70, 120)
QuantDataWorkSheet <- quantile(scores) ; QuantDataWorkSheet

IQR(scores)

55.00 - 8.25
63.50 + 8.25



#################2 variables at once########################
data("mtcars")
summary(mtcars)

mtcars <- mtcars %>% 
  mutate(vs=as.factor(vs)) %>%
  mutate(gear=as.factor(gear))
  
countsDF <- mtcars %>%
  group_by(vs, gear) %>% 
  count()
countsDF

##stacked bar chart
ggplot(countsDF, aes(gear, n, fill=vs)) + geom_bar(stat="identity")

##grouped bar chart
ggplot(countsDF, aes(gear, n, fill=vs)) + geom_bar(stat="identity", position="dodge") + labs(title="Car Distribution by VS and Gear", y = "Count", x = "Number of Gears") + theme_dark()

##average calculation in a chart
avg_mpg <- mtcars %>% 
  group_by(vs, gear) %>% 
  summarise(mpg=mean(mpg, na.rm=T))
avg_mpg

ggplot(avg_mpg, aes(gear, mpg, fill=vs)) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c("yellow","brown"))



################Multiple BoxPlots##########################

ggplot(mtcars, aes(gear, mpg)) + geom_boxplot()

ggplot(mtcars, aes(gear, mpg)) + geom_boxplot() + coord_flip()


#add parameters

ggplot(mtcars, aes(gear, mpg, fill=gear)) + geom_boxplot() + scale_fill_manual(values=c("gray", "red","darkblue")) + theme_light()

ggplot(mtcars, aes(vs, mpg, fill=vs)) + geom_boxplot(show.legend=F) + scale_fill_manual(values=c(4,7))



#####################Scatter Plot######################

Edu <- read.csv("data/Education.csv")
summary(Edu)
plot(Edu$Education, Edu$Income)

plot(Edu$Income ~ Edu$Education)

ggplot(Edu, aes(Education, Income)) + geom_point() + labs(y="Education", x="Income")

##add properties

ggplot(Edu, aes(Education, Income)) + geom_point(color="#183028", shape=10) + labs(x="Education", y="Income")

ggplot(Edu, aes(Education, Income)) + geom_point() + labs(x="Education", y="Income") + geom_line(color="#789F90")

ggplot(Edu, aes(Education, Income)) + geom_point() + labs(x="Education", y="Income") + geom_smooth(method="lm", color="#789F90")

#########hp and mpg

ggplot(mtcars, aes(hp, mpg)) + geom_point() + geom_smooth(method="lm")
###Strong and negative


##qsec and mpg
ggplot(mtcars, aes(qsec, mpg)) + geom_point() + geom_smooth(method="lm")
##Weak and positive

ggplot(mtcars, aes(disp, mpg)) + geom_point() + geom_smooth(method="lm")
##Strong and Negative

ggplot(mtcars, aes(cyl, mpg)) + geom_point() + geom_smooth(method="lm")
##not best image because only three categories of cylinders (4,6,8)

mtcars <- mtcars %>%
  mutate(cyl=as.factor(cyl))

summary(mtcars$cyl)

#after it is correctly made into a factor, we can run a multiple boxplot
ggplot(mtcars, aes(cyl, mpg)) + geom_boxplot(fill="lightgreen")

library(MASS)
data("UScrime")
summary(UScrime)


ggplot(UScrime, aes(Ed, Time)) + geom_point() + geom_smooth(method="lm")


coffee <- read.csv("data/day4.csv",na.strings = "", stringsAsFactors = T)

summary(coffee)
drop_na(coffee)
ggplot(coffee, aes(Aroma, Flavor)) + geom_point(color="#6F4E37") + geom_smooth(method="lm", color="black")

ggplot(coffee, aes(Processing.Method, TotalScore, fill=Processing.Method)) + geom_boxplot() + scale_fill_manual(values=c("gray", "red","darkblue","#6F4E37","black","yellow")) + theme_light()+labs(x="Processing Method", y="Total Score")


ggplot(coffee, aes(TotalScore)) + geom_density(color = "#2A4533", fill="#6F4E37", alpha=.8) +
  geom_vline(aes(xintercept = mean(TotalScore)), color="red", linetype="dashed", lwd=.5) + labs(x="Total Score", y="Density Distribution of Score")


