#' ---
#' title: "Assignment 4"
#' 
#' output:
#'    html_document:   
#'      toc: true  
#'      toc_depth: 5 
#'      theme: united
#'      dev: 'svg'
#'      number_sections: false
#' ---
#'
options(warn=-1)
library(readxl)
library(EnvStats)
models <- read_excel('models.xlsx')
data <- read_excel('data.xlsx')
k <- 2
#'
#' # Example 1
#' 
#' Find out graphically what the structure of saved data is. 
#' Verify that the model is safe with a suitable parametric test. 
#' What is the null and alternative hypothesis? 
#' Test at the significance level 𝛼 = 0.05. 
#' Evaluate the results of the investigation. 
#' Is the model safe?
#' 
#' 𝑘 = 2 model MC LoAII
#' 
#' **Solution** 
#' 
#' $H_{0}$, null hypothesis - proportion, exp/model is less, equal than 1, <= 1
#' 
#' $H_{1}$, alternative hypothesis - proportion, exp/model is greater than 1, > 1, model is safe
#'  
alpha <- 0.05 # significance level

v1 <- models$MCLoAII
v2 <- models$LoAIIexp

boxplot(v1, v2, xlab="", ylab="Value", names = c('MCLoAII', 'LoAIIexp'), col="yellow")
#' From the boxplot, we can see that exp(LoAIIexp) has more concentrated values concentrated 
#' since it is narrower box and has one lower outlier, also has shorter whiskers, 
#' unlike the model (MCLoAII), which is wider box and has longer whiskers, 
#' also has two upper outliers
#' 
#' We can also notice that the model has a larger maximum and a smaller minimum than exp.
#' 
#' The median for exp is roughly in the middle of the box, but the model has a median lower than the middle of the box.
#' 
hist(v1, breaks = seq(min(v1), max(v1), len=20),main="Histogram - model", xlab="Values",ylab="Quantity", col="green")
hist(v2, breaks = seq(min(v2), max(v2), len=20),main="Histogram - exp", xlab="Values",ylab="Quantity", col="blue")
#' From the histograms, we can see that the model achieve more values around 1.0 
#' and also few values above 1.5, unlike exp, which has concentrated values close to each other, 
#' in the range of 1.0 to 1.4

max(v1) # max model
max(v2) # max exp

min(v1) # min model
min(v2) # min exp

mean(v1) # mean model
mean(v2) # mean exp

median(v1) # median model
median(v2) # median exp

# model
quantile(v1, probs=0.25) # lower quartile, 25% of the data have a smaller or equal value
quantile(v1, probs=0.75) # upper quartile, 75% of the data have a smaller or equal value
IQR(v1) # interquartile range - the difference between the upper and lower quartiles

# exp
quantile(v2, probs=0.25) # lower quartile, 25% of the data have a smaller or equal value
quantile(v2, probs=0.75) # upper quartile, 75% of the data have a smaller or equal value
IQR(v2) # interquartile range - the difference between the upper and lower quartiles
#' 
#' 
#' T-Test, alpha value = 0.05
t.test(v2/v1, mu = 1, alternative = "greater", conf.level = 1 - alpha)
#' p value is equal to 0.0005005, it is less than alpha
#' 
#' We reject the null hypothesis $H_{0}$ and we can say that the model is safe, 
#' we accept the alternative hypothesis $H_{1}$
#' 
#'
k <- 6
#'
#' # Example 2
#' 
#' The table shows the number of students absent from school during weekdays. 
#' We assume that there is no reason for children to be absent on some days.
#' Choose an appropriate theoretical distribution whose agreement with the data 
#' distribution you will test.
#' 
#' **Solution** 
#' 
data_tab<-c(rep('monday', 125-k), rep('tuesday', 88+k), rep('wednesday', 85),rep('thursday', 94), rep('friday', 108))
barplot(table(data_tab), col="yellow")
#' From the graph we can see that the values on individual days are not significantly different
prob<-rep(1/5,5) # theoretical distribution, 1/5 is the probability of missing students for each day
prob
#' 
#' $H_{0}$, data comes from the theoretical distribution
#' 
#' $H_{1}$, data does not come from a theoretical distribution
#' 
chisq.test(table(data_tab),p=prob) 
#' p value is greater than 0.05, we do not reject $H_{0}$, we cannot say that 
#' the data does not come from the theoretical distribution
#' 
#' We cannot say that students are absent on some day more than other days
#'
#'
#' # Example 3
#' 
#' When monitoring the lifetime of 33 bulbs for data projectors, the following 
#' results were found (operating time per time unit):
#' 
#' At a significance level of alpha=0.1, test these hypotheses that the selection is
#' 
#' * by choosing from an exponential distribution with an expected value of 25. 
#' Draw a histogram of relative frequencies and fit the density curve - empirical 
#' and theoretical.
#' 
#' **Solution** 
#' 
values <- c(7+k, 15, 16, 17, 19, 20, 21, 21, 22, 23, 23, 23, 23, 24, 24, 24, 24, 24,
       25, 25, 26, 27, 28, 29, 29, 30, 30, 31, 32, 33-k, 34, 34, 35)
l <- 1/25

hist(values, freq = FALSE, col="blue", main = "Histogram of relative frequencies ") 
lines(density(values), lwd = 2, col = "green") # empirical density
lines(x = values, y = dexp(values, rate = l), col = "orange") # theoretical density
#' The green curve describes the empirical density
#' 
#' The orange curve describes the theoretical density
#' 
#' $H_{0}$, data comes from the theoretical distribution
#' 
#' $H_{1}$, data does not come from the theoretical distribution
#' 
ks.test(values,"pexp", l)
#' p value is less than 0.1, $H_{0}$ can be rejected, the data does not come 
#' from the theoretical distribution
#' 
#' * by sampling from a normal distribution with unknown parameters. 
#' Draw a histogram of relative abundances and fit the density curve - empirical and theoretical. 
#' Draw a Q-Q graph.
#' 
s = seq(min(values), max(values), length.out = length(values))
hist(values, freq = FALSE, col="blue", main = "Histogram of relative frequencies")
lines(density(values), lwd = 2, col = "green") # empirical density
lines(s, dlnorm(s, meanlog = mean(log(values)), sdlog = sd(log(values))), col = "orange") # theoretical density
#' The green curve describes the empirical density
#' 
#' The orange curve describes the theoretical density
#' 
qqnorm(values)
qqline(values, lwd = 2)
#' 
#' $H_{0}$, data comes from a normal distribution
#' 
#' $H_{1}$, data does not come from a normal distribution
#' 
shapiro.test(values)
#' p value is greater than 0.1, we do not reject $H_{0}$, we cannot say 
#' that the data does not come from a normal distribution
#'
#' # Example 4
#' 
#' We compared the length of effectiveness of antiparasitic collars of two brands 
#' designed for smaller breeds of dogs. Effectiveness measurements in days are 
#' given by the table. At the α=0.05 significance level, test the hypothesis that 
#' the samples are from the same distribution. If the null hypothesis is rejected, 
#' decide which brand you would prefer if the collar prices of both brands are the same. 
#' Compare also graphically.
#' 
#' **Solution** 
#' 
a <- c(94, 86, 90, 92, 91, 75+k, 86, 87, 97, 91, 92, 97)
b <- c(84, 97, 89, 94, 81+k, 83, 90, 96, 100, 82)

boxplot(a, b, main="Charts of antiparasitic collars", names = c('The first type of collar','The second type of collar'))
#' From the graph, we can see that the second type of collar has a larger 
#' interquartile range than the first type of collar, also the second type of 
#' collar reaches a larger maximum than the first type. The median has a larger 
#' first type of collar than the second type
#' 
par(mfrow=c(1,2))
hist(a, xlab="A", col="purple", main="First type of collar")
hist(b, xlab="B", col="orange", main="Second type of collar")
#' 
#' $H_{0}$, data come from a similar distribution
#' 
#' $H_{1}$, data does not come from a similar distribution
#' 
ks.test(a, b)
#' p value is greater than 0.05, we do not reject $H_{0}$, we cannot say that 
#' the data does not come from a similar distribution
#' 