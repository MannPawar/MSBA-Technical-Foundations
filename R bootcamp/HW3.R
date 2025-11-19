rm(list=ls())

######################Mann Pawar#########################

library(tidyverse)

#Q1

Q1 <- read.csv("data/HW3.csv")

#Q2

Q2 <- nrow(Q1)  #594

#Q3

Q3 <- Q1 %>% filter(salary_in_usd > 300000) %>% nrow()  #11

#Q4

Q4 <- Q1 %>% filter(salary_in_usd > 300000, remote_ratio == 100) %>% nrow() #3

#Q5
Q5 <- na.omit(Q1)

#Q6
Q6<-nrow(Q5)  #594-579 = 15 rows were deleted after the na.omit()

#Q7
Q7 <- arrange(Q1, desc(salary_in_usd)) #AI Scientist

#Q8
Q1 <- Q1 %>% mutate(Q8 = salary_in_usd/1000)

#Q9
Q1 <- Q1 %>%  mutate(Q9 = case_when(
  remote_ratio == 100 ~ "Remote",
  remote_ratio > 0 & remote_ratio < 100 ~ "Hybrid",
  remote_ratio == 0 ~ "On-site"
))

#Q10
Q10 <- Q1 %>% group_by(experience_level) %>% summarize(avg_salary = mean(salary_in_usd, na.rm = T))
## A tibble: 4 × 2
#experience_level      avg_salary
#  <chr>                  <dbl>
#  1 EN                  103978.
#  2 EX                  187711.
#  3 MI                  115563.
#  4 SE                  159531.
