rm(list=ls())

###########Calculus and Logic######################
library(tidyverse)

c <- c(2,4,5,6,7,8,10,12,15,20,23,25,30,35,40,50,70,90,120,150)

log_values <- log(c)
hist(c)
hist(log_values)

bindedclog <- data.frame(c, log_values)

chist<-ggplot(bindedclog, aes(c)) + geom_histogram(bins = 5)
loghist<-ggplot(bindedclog, aes(log_values)) + geom_histogram(bins = 5)

gridExtra::grid.arrange(chist, loghist) 



##############Derivatives###############
##define function and its derivative


f <- function(x) x^2

df <- data.frame(x=seq(-5, 5, by=.1))
df$fx <- f(df$x)

###make some key points to show on the chart
key_points <- data.frame(x=c(-2,0,2), fx=f(c(-2,0,2)))

ggplot(df, aes(x, fx)) + geom_line(color="blue") + geom_point(data=key_points, aes(x, fx), color="red") + geom_vline(xintercept = c(-2,0,2), linetype = "dashed", color="gray") + theme_minimal() + labs(y="f(x)",title="Plot of f(x)=x^2")


########Using expression and D function in R to compute derivatives
expr <- expression(x^2) ; expr
D(expr, "x")


###limit formula
x <- 2
h <- .000001
(f(x+h) - f(x))/h
#4.000001

##making f prime
f_prime <- function(x) 2*x
##add to dataset
df$fpx <- f_prime(df$x)

##plot
ggplot(df, aes(x)) + geom_line(aes(y=fx, color="f(x) = x^2")) + geom_line(aes(y=fpx, color="f'(x)=2*x")) + theme_minimal()
h <- 0.0001
g <- function(x) 3*(x)^3 + 2*x
(g(1 + h) - g(1))/h

h <- function(x) 2*x^2 - 5*x + 1 
(h(2 + 0.0001) - h(2))/0.0001

j <- function(x) exp(x)
(j(0 + 0.0001) - j(0))/0.0001

k <- function(x) log(x)
(k(2 + 0.0001)-k(2))/0.0001

factorial(5)
choose(5, 2)


###########################################
p <- function(x) -2*x^2 + 12*x + 20
t <- 1:5
h <- 0.00001
slope_approx <- (p(t+h) - p(t))/h
round(slope_approx)
#8  4  0 -4 -8

expr <- expression(-2*x^2 + 12*x + 20)
D(expr, "x")
12 - 2 * (2 * t)
#8  4  0 -4 -8

time <- seq(0,6,0.1)
df = data.frame(time,P=p(time))
df

ggplot(df, aes(time, P)) + geom_line(color="#777abc")


##Gradients

tim <- expression(x^2 + y^2)

##Partial Derivative wrt x
D(tim, "x")   #2 * x
##Partial Derivative wrt y
D(tim, "y")   #2 * y

###at point(1,2) = [2, 4]
2 * 1  #2
2 * 2  #4

library(plotly)
#Generate Grid

grid <- expand.grid(x = seq(-3, 3, length.out=30),
                    y = seq(-3, 3, length.out=30)) %>% 
  mutate(z = x^2 + y^2,
         dx = 2*x,
         dy = 2*y)
#highlight point (1,2)
pt <- data.frame(x=1,y=2,dx=2,dy=4)
# Create 3D surface using plotly
x <- seq(-3, 3, length.out = 50)
y <- seq(-3, 3, length.out = 50)
z <- outer(x, y, function(x, y) x^2 + y^2)
plot_ly(x = ~x, y = ~y, z = ~z) %>%
  add_surface(colorscale = "Viridis") %>%
  layout(title = "3D Surface of f(x, y) = x^2 and y^2",
         scene = list(xaxis = list(title = "x"),
                      yaxis = list(title = "y"),
                      zaxis = list(title = "f(x, y)")))


#Counting Rules
factorial(9)
#362880

factorial(0)
#1

##little league
##selecting 9 from 12 - permutation with no repetition
factorial(12)/factorial(3)

##combination with no repetition
factorial(12)/(factorial(3)*factorial(9))
#220
choose(12, 9)
#220

##ice cream example 4 flavors(n) and 3 scoops(r)
factorial(3+4-1)/(factorial(3)*factorial(4-1))
#20

options(scipen=999)
factorial(16)

16*15*14
#3360
factorial(16)/factorial(13)
#3360

factorial(6)

factorial(10)/factorial(8)
