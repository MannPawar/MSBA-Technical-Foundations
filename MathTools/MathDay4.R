rm(list=ls())

#####Linear Algebra##############

v <- c(2, -1, 3)
##calculate magnitude
magnitude <- sqrt(sum(v^2)) ; magnitude  #3.741657


u <- c(2,3)
v <- c(4,1)
u %*% v #11
sum(u*v)  #11

##Cosine Similarity example
#Dot product is numerator
u <- 1:2
v <- 2:3
u %*% v  #Dot Product == 8
sum(u*v)  #8

#Calculate Magnitude (denominator)
sqrt(sum(u^2)) * sqrt(sum(v^2))  #8.062258

#divide dot product by magnitude(u) * magnitude(v) to get the cosine similarity
8/8.062258  #0.9922778

##second example cosine similarity
u <- c(3,0,1)
v <- c(6,0,2)

#compute dot product
u %*% v  #20

#compute magnitude
sqrt(sum(u^2)) * sqrt(sum(v^2))  #20

#divide dot product by magnitude to get cosine similarity
20/20    #1   directionally identical vectors and very high similarity

movie <- read.csv("data/movie.csv", row.names = 1)
str(movie)

##define function for cosine similarity
cosine_similarity <- function(x,y){
  sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}

cosine_similarity(u,v)  #1

cosine_similarity(movie["Inception", ], movie["The Matrix", ]) #0.8660254

cosine_similarity(movie["Die Hard", ], movie["Titanic", ])  #0

cosine_similarity(movie["The Notebook", ], movie["Titanic", ]) #1

cosine_similarity(movie["The Notebook", ], movie["Toy Story", ]) #0.4082483


Qreturns <- c(2, 3.5, -1, 4)
sqrt(sum(Qreturns^2)) #5.766281

A <- c(3,4) ; B <- c(6,8)
A %*% B   #13
sqrt(sum(A^2)) * sqrt(sum(B^2))  #17.02939
50/50


#########Matrices#########

##Identity Matrix
I <- diag(3); I

###Adding two matrices

A <- matrix(1:4, 2, 2, byrow = T) ; A
B <- matrix(5:8, 2, 2, byrow = T) ; B

A + B

###scalar multiplication
C <- matrix(c(2, -1, 0, 4), 2, 2, byrow = T) ; C
3*C


###Matrix multiplication
A <- matrix(c(2,1,0,3),2,2,byrow=T) ; A
B <- matrix(c(4,5),2,1) ; B

A %*% B


###Identity Matrix in Multiplication
D <- matrix(2:5, 2, 2, byrow=T); D
E <- diag(2); E

D %*% E

###Transpose a matrix using t() function
G <- matrix(1:6, 3, 2, byrow=T); G
t(G)

###Inverse of a matrix
H <- matrix(c(4, 7, 2, 6), 2, 2, byrow=T)
solve(H)  ##Inverse matrix 


####Identity Matrix and Inverses
round(solve(H) %*% H, 0)  #Multiplying Matrix with its Inverse returns the Identity Matrix


A <- matrix(c(2,0,1,3), 2, 2, byrow=T) ; A
B <- matrix(c(4,5),2,1) ; B

A %*% B

C <- matrix(c(2,1,0,6),2,2, byrow=T) ; C
D <- matrix(c(4,5),2,1, byrow=T) ; D

C %*% D

J <- matrix(c(1:6), 2, 3, byrow = T) ; J
K <- matrix(c(1,0,-1),3, 1) ; K
J %*% K

###System of Equations

J <- matrix(c( 1, 2, 1, 2, 3, 3, 1, 1, 1), nrow=3, byrow=T) ; J
K <- c(6,14,8) ; K

solve(J, K)


M <- matrix(c(1, 1, 1, 2, -1, 3, 4, 2, -1), nrow = 3, byrow=T) ; M
N <- c(9,1,5) ; N

solve(M, N)


###Eigenvalue############

L <- matrix(c(4,2,1,3), 2, 2, byrow=T)
eigen(L)

A <- matrix(c(5,2,1,4), 2, 2, byrow=T); A
eigen(A)

###########PCA#################
data("iris")
str(iris)

library(tidyverse)

#removing the species variable
iris_numeric <- iris %>% select(-Species)
str(iris_numeric)

#scale the data
iris_scaled = scale(iris_numeric)

#run the PCA with prcomp()
#center and scale are parameters that mean center in place of the scale() function, only need one or the other
iris_pca <- prcomp(iris_scaled,center=TRUE,scale.=TRUE)
summary(iris_pca)


pcaDF <- as.data.frame(iris_pca$x)
pcaDF$Species <- iris$Species

ggplot(pcaDF, aes(PC1, PC2, color=Species)) + geom_point()

plot(iris_pca, type="l")

data("USArrests")
str(USArrests)

usarrests_scaled <- scale(USArrests)

pca_arrests_result <- prcomp(usarrests_scaled)

summary(pca_arrests_result)

plot(pca_arrests_result, type="l")


sim <- read.csv("data/sim_data.csv", row.names = 1)

pca_sim <- prcomp(sim, scale. = T)
summary(pca_sim)

#####Singular Value Decomposition#########
A <- matrix(c(4,5,3,2,5,4), nrow=3, byrow=T)
A

svd_result <- svd(A)
svd_result

U <- svd_result$u ; U

Sigma <- diag(svd_result$d) ; Sigma
##with two zeroes and two sigmas, the matrix is rank 2.
## 2 non zeros - rank is 2.

V <- svd_result$v ; V

library(imager)
library(jpeg)

img <- load.image("data/wren.jpg")

##convert to grayscale
gray_image <- grayscale(img)

#convert to matrix

A <- as.matrix(gray_image)

svd_result <- svd(A) 
##Images are too high dimension to see output clearly this way

##reconstruct image using top K singular values
reconstruct_image <- function(k){
  U_k <- svd_result$u[, 1:k]
  D_k <- diag(svd_result$d[1:k])
  V_k <- svd_result$v[, 1:k]
  U_k %*% D_k %*% t(V_k)
}

##Plot compressed images for different values of k

par(mfrow=c(1,3)) ##1 row, 3 column output in the plot window

for(k in c(10,50,100)) {
  img_k <- reconstruct_image(k)
  image(t(apply(img_k, 2, rev)), col=gray.colors(256), main=paste("k=",k), axes=FALSE)
}


A <- matrix(c(4,5,3,2,5,4),3,2,byrow=T)

svd_A <- svd(A)

U <- svd_A$u ; U
Sigma <- svd_A$d ; Sigma
V <- svd_A$v ; V

A <- c(6, -4) ; B <- c(2,5)
A %*% B

A <- matrix(c(7,-2,4,5),2,2,byrow=T) ; A
B <- matrix(c(3,-1)) ; B

A %*% B

A <- matrix(c(3,2,1,-2,1,4,1,7,4),3,3,byrow=T) ; A
B <- c(7,1,10); B
solve(A,B)

data("diamonds")
summary(diamonds)
diamonds_truncated <- diamonds %>% select(-cut,-color,-clarity)

diamonds_scaled <- scale(diamonds_truncated)

pca_diamond <- prcomp(diamonds_scaled, scale.=T)

summary(pca_diamond)

