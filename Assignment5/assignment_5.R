#' ---
#' title: "Assignment 5"
#' output:
#'    html_document:   
#'      toc: true  
#'      toc_depth: 5 
#'      theme: united
#'      dev: 'svg'
#'      number_sections: false
#' ---
#'
library("randtests")
library("readxl")
library("dunn.test")
library("moments")
k <- 2
#'
#' # Example 1
#' 
#' A quality control worker at a paint plant intercepted and measured the 
#' weight of 16 tin cans coming out of a new conveyor belt filling line.
#' 
#' Are there any systematic faults?
#' 
#' **Solution** 
#' 
x <- c(68.2, 71.6, 69.3, 71.6, 70.4, 65.0, 63.6, 64.7, 65.3, 64.2, 67.6, 68.6, 66.8, 70.1)

boxplot(x, ylab="Weights", col='yellow') 

#' We can read the data distribution from the boxplot

plot(density(x))

#' Density graph
#' 
#' $H_{0}$, data is random
#' 
#' $H_{1}$, data is not random

runs.test(x, plot=T)

#' Wald - Wolfowitz test - we will find out if the data is random or not
#' 
#' p value is greater than 0.05, we do not reject $H_{0}$ and therefore cannot 
#' say that the data is random
#' 
#' $H_{0}$, data do not have significant skewness
#' 
#' $H_{1}$, data have significant skewness

agostino.test(x) 

#' Agostino test for skewness 
#' 
#' p value is greater than 0.05
#' 
#' We do not reject $H_{0}$ and we cannot say that the data have a statistically 
#' significant skewness
#' 
#' From the graphs and tests we can see that the data is random and not skewed
#' 
#' We cannot say that there are systematic failures on the line
#'
#' # Example 2
#' 
#' The company compared the financial costs of advertising in four major magazine 
#' publishers. For each publisher 7 magazines were randomly selected and 
#' their advertising costs were determined in thousands of crowns. It is necessary 
#' to verify whether there are differences in advertising costs between these four publishers.
#' 
#' **Solution** 
#' 
A <- c(57, 65, 50, 45, 70, 62, 38)
B <- c(81, 72, 64, 55, 90, 38, 75)
C <- c(35, 62, 58, 59, 46, 60, 61)
D <- c(73, 85, 92, 68, 82, 94, 66)

a <- c(A, B, C, D)
b <- c(rep('A', 7), rep('B', 7), rep('C', 7), rep('D', 7))

boxplot(a~b, xlab = "Publisher", ylab="Finance", col="orange")
#' From the boxplot, we can see the different expenses of publishers per magazine
#'
#' $H_{0}$, medians of the publishers are equal
#' 
#' $H_{1}$, medians of the publishers are not equal

kruskal.test(factor(a),factor(b)) 
#' p value is less than 0.05
#' 
#' We can reject $H_{0}$, the publishers medians are not equal
#' 
#' We can perform the Dunn test, testing individual pairs against each other

dunn.test(a, b, label = T, altp = T)

#' We see that there is a statistically significant difference between publishers 
#' D and A and also between publishers D and C
#'
#' We can say that publisher D invests statistically significantly more funds in 
#' advertising than publishers A and C
#'