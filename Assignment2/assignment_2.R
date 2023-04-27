#' ---
#' subtitle: "Descriptive statistics"
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
#' The data represents the net annual income of 235 persons. Except income, we also have information about
#'
#' * age category (age): 1: 18-25y, 2: 26-35y, 3: 36-45y, 4: 46-55y, 5: 56+
#' 
#' * highest level of education (education): 1: primary or secondary school
#' without high school diploma, 2: high school with high school diploma, 3: university 1st degree, 4: university
#' 2nd degree, 5: university 3rd degree
#' 
#' * place - performance of work (place): 1: small village, 2: small town, 3: district town, 4: regional town
#' 
#' * sector: 1: services, 2: production, 3: trade
#'
options(warn=-1)
library("moments")
library("vioplot")
data <- read.csv("data.csv")
k <- 6
#' 
#' # Example 1
#' 
#' Using descriptive statistics, describe and perform primary
#' analysis for the INCOME variable, regardless of the other variables. Describe the results
#' and show graphically with suitable types of graphs, which you also describe.
#' 
#' **Solution**
#' 
#' All results refer to values from the Income column
#'
#'  Max
max(data$Income) # maximum value
#' Min
min(data$Income) # minimum value
#' Average
mean(data$Income) # average
#' Modus
sort(table(data$Income)) # modus
#' Median
median(data$Income) # median
#' Dispersion
var(data$Income) # dispersion of values
#' Standard deviation
sd(data$Income) # distribution of numbers around the mean, the smaller is closer to the mean
#' Interquartile range
quantile(data$Income,probs = 0.75) # upper quartile, 75% of the data have greater or equal value
quantile(data$Income,probs = 0.25) # lower quartile, 25% of the data have smaller or equal value
#'
quantile(data$Income,probs = 0.5) # median
IQR(data$Income) # interquartile range - the difference between the upper and lower quartiles
#' Variation coefficient
sd(data$Income)/mean(data$Income)*100 #the ratio of the standard deviation to the mean, it is a standardized measure of the variance of a distribution
#' Graph
boxplot(data$Income,main="Boxplot - Income", xlab="", ylab="Value", col="red") 
#' From this graph we can see outliers, specifically in this case we see one upper outlayer, further we can see the median, interquartile range, upper and lower quartile, we can also notice that this boxplot is narrow and wide so most of the values are concentrated around the median
#'
#' Measures of asymmetry
skewness(data$Income) # says, that the greater concentration of values is on the right side
kurtosis(data$Income) # says, that less data is found on the tails, more flat than pointed
hist(data$Income, breaks = seq(min(data$Income), max(data$Income), len=45),main="Histogram - Income", xlab="Income",ylab="Frequency", col="brown")
plot(density(data$Income), main="Density graph - Income", xlab="Income")
#' From the histogram and density graph, we can see that less data is located at the tails of the graph
#'
#' Analysis of outliers - Tukey's test
Q1<-quantile(data$Income, probs=0.25)
Q3<-quantile(data$Income, probs=0.75)
IQR<-IQR(data$Income)
k1 <- 1.5
k2 <- 3
#' Deviated values
(data$Income<(Q1-k1*IQR)) # does not have a lower deviated value
which(data$Income<(Q1-k1*IQR)) 
(data$Income>(Q3+k1*IQR)) # upper deviated value found
which(data$Income>(Q3+k1*IQR))# index
data$Income[data$Income>(Q3+k1*IQR)] # value
#' Extreme values
(data$Income<(Q1-k2*IQR)) # does not have a lower extreme value
which(data$Income<(Q1-k2*IQR))
(data$Income>(Q3+k2*IQR)) # does not have a upper extreme value
which(data$Income>(Q3+k2*IQR))
#' 
#' # Example 2
#' 
#' Perform an initial statistical analysis for the Income variable
#' given the two levels of education, describe the Income variable for each level separately.
#' Describe the results and display them graphically with appropriate types of graphs, which you also describe.
#' Compare the income given the two levels (categories) of education.
#' 
#' Level 3 and Level 4
#' 
#' **Solution** 
data_3 <- unlist(data$Income[data$Education == 3])
data_4 <- unlist(data$Income[data$Education == 4])
#' Number of levels
print(length(data_3))
print(length(data_4))
#' Level 3 has more values
#' 
#' Max
max(data_3) # maximum value of level 3
max(data_4) # maximum value of level 4
#' 
#' Min
min(data_3) # minimum value of level 3
min(data_4) # minimum value of level 4
#'  
#'  Average
mean(data_3) # average level 3
mean(data_4) # average level 4
#'
#'  Modus
sort(table(data_3)) # modus for level 3
sort(table(data_4)) # modus for level 4
#' 
#' Median
median(data_3) # mean value for level 3
median(data_4) # mean value for level 4
#' 
#' Variance
var(data_3) # variance of level 3 values
var(data_4) # variance of level 4 values
#' 
#' Standard deviation
sd(data_3) # measure of the distribution of numbers around the mean for level 3
sd(data_4) # measure of the distribution of numbers around the mean for level 4
#'
#' Interquartile range
#' 
#' Level 3
quantile(data_3,probs = 0.75) # upper quartile, 75% of the data have a less or equal value
quantile(data_3,probs = 0.25) # lower quartile, 25% of the data have a smaller or equal value
quantile(data_3,probs = 0.5) # median
IQR(data_3) # interquartile range - the difference between the upper and lower quartiles
#' Level 4
quantile(data_4,probs = 0.75) # upper quartile, 75% of the data have a less or equal value
quantile(data_4,probs = 0.25) # lower quartile, 25% of the data have a smaller or equal value
quantile(data_4,probs = 0.5) # median
IQR(data_4) # interquartile range - the difference between the upper and lower quartiles
#' 
#' Graph
par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)
hist(data_3, breaks = seq(min(data_3), max(data_3), len=50),main="Histogram - Level 3", xlab="Income",ylab="Frequency", col="purple")
hist(data_4, breaks = seq(min(data_4), max(data_4), len=15),main="Histogram - Level 4", xlab="Income",ylab="Frequency", col="green")
plot(density(data_3), main="Density - Level 3", xlab="Income", col="purple")
plot(density(data_4), main="Density - Level 4", xlab="Income", col="green")
dev.off()
#' From the histograms and density graphs we can see the data distribution that level 3 has less data on the tails of the graph unlike level 4
#'
#' Measures of asymmetry
skewness(data_3) # says, that the greater concentration of values is on the left side
skewness(data_4) # says, that the greater concentration of values is on the left side
kurtosis(data_3) # says, that little data is found on the tails, more pointed
kurtosis(data_4) # says, that the data is also found on the tails, less pointed rather flat
#' 
#' Graph
boxplot(data_3, data_4, xlab="", ylab="Value", col="red") 
axis(side=1,at=(1:2),labels=c("Level 3","Level 4"), line = 0, tick=FALSE)
#' From this graph we can see that level 3 has more scattered values unlike level 4, but level 4 has a slightly larger interquartile range, further we can see outliers, specifically in this case we see that level 3 has them above and below but level 4 no, a similar median is visible
#' 
#' Analysis of outliers - Tukey's test
#' 
#' Deviated values in the data
#' 
#' Level 3
Q1<-quantile(data_3, probs=0.25)
Q3<-quantile(data_3, probs=0.75)
IQR<-IQR(data_3)
k1<-1.5

(data_3<(Q1-k1*IQR)) # has lower offset values
which(data_3<(Q1-k1*IQR)) # index
data_3[data_3<(Q1-k1*IQR)] # value

(data_3>(Q3+k1*IQR)) # has upper offset values
which(data_3>(Q3+k1*IQR))# index
data_3[data_3>(Q3+k1*IQR)] # value
#' Level 4
Q1<-quantile(data_4, probs=0.25)   # lower quartile
Q3<-quantile(data_4, probs=0.75)  # upper quartile
IQR<-IQR(data_4)

(data_4<(Q1-k1*IQR)) # does not have a lower offset value
which(data_4<(Q1-k1*IQR))
(data_4>(Q3+k1*IQR)) # does not have an upper offset value
which(data_4>(Q3+k1*IQR))
#'
#' Extreme values in the data
#' 
#' Level 3
Q1<-quantile(data_3, probs=0.25)
Q3<-quantile(data_3, probs=0.75)
IQR<-IQR(data_3)
k2 <- 3

(data_3<(Q1-k2*IQR)) # has no lower extreme value
which(data_3<(Q1-k2*IQR)) # index

(data_3>(Q3+k2*IQR)) # has no upper extreme value
which(data_3>(Q3+k2*IQR))# index
#' Level 4
Q1<-quantile(data_4, probs=0.25)   # lower quartile
Q3<-quantile(data_4, probs=0.75)  # upper quartile
IQR<-IQR(data_4)

(data_4<(Q1-k2*IQR)) # has no lower extreme value
which(data_4<(Q1-k2*IQR))
(data_4>(Q3+k2*IQR)) # has no upper extreme value
which(data_4>(Q3+k2*IQR))
#' 
#' # Example 3
#' 
#' 2. place
#' 
#' Graphically compare income with respect to different levels (categories) of the variable
#' 
#' **Solution** 
data_place1 <- unlist(data$Income[data$Place == 1])
data_place2 <- unlist(data$Income[data$Place == 2])
data_place3 <- unlist(data$Income[data$Place == 3])
data_place4 <- unlist(data$Income[data$Place == 4])

boxplot(data_place1, data_place2, data_place3, data_place4, xlab="", ylab="Value", col="yellow")
axis(side=1,at=(1:4),labels=c(" ","Small town", " ", "Regional town"), line = 0, tick=FALSE)
axis(side=1,at=(1:4),labels=c("Small village"," ", "District town", " "), line = 1, tick=FALSE)
#' From the graph, it can be seen that the median gradually increases from the Small village to the Regional town, if we compare the Small village and the Regional town, we see that the maximum value has increased and the minimum value has decreased, in the District town and the Regional town, we see that the median is above the value 14,000 different from the Small Village and the Small town

vioplot(data_place1, data_place2, data_place3, data_place4, xlab="", ylab="Value", col="blue")
axis(side=1,at=(1:4),labels=c(" ","Small town", " ", "Regional town"), line = 1, tick=FALSE)
axis(side=1,at=(1:4),labels=c("Small village"," ", "District town", " "), line = 2, tick=FALSE)
#' This graph shows the frequency of values, the wider, the greater the frequency of a given value for a specific place, values rise from the Small village to the Regional town
#'
#' # Example 4
#' 
#' 2. age
#' 
#' * age category (age): 1: 18-25y, 2: 26-35y, 3: 36-45y, 4: 46-55y, 5: 56+
#' 
#' Construct a table of frequency, relative frequency, pie chart
#' and bar chart, which will provide information on the number of respondents within
#' individual levels (categories) of the variable
#' 
#' **Solution** 
data_age1 <- subset(data, Age == 1)
data_age2 <- subset(data, Age == 2)
data_age3 <- subset(data, Age == 3)
data_age4 <- subset(data, Age == 4)
data_age5 <- subset(data, Age == 5)
#' Frequency
sort(table(data$Age))
#' Relative frequency
a1 <- round(table(data_age1$Age)/length(data$Age)*100, 2)
a2 <- round(table(data_age2$Age)/length(data$Age)*100, 2)
a3 <- round(table(data_age3$Age)/length(data$Age)*100, 2)
a4 <- round(table(data_age4$Age)/length(data$Age)*100, 2)
a5 <- round(table(data_age5$Age)/length(data$Age)*100, 2)
l <- c("18-25y", "26-35y", "36-45y", "46-55y", "56+")
lbls <- paste(l, c(a1, a2, a3, a4, a5))
lbls <- paste(lbls,"%",sep="")
pie(c(a1, a2, a3, a4, a5), main="Pie chart - Relative frequency of individual age groups", labels = lbls)
#'
barplot(data_len_age, main="Bar graph - Frequency of individual age groups", names.arg=c("18-25y", "26-35y", "36-45y", "46-55y", "56+"), col="orange")
pie(data_len_age, main="Pie chart - Number of individual age groups", labels = l)
#'From the graphs, we can see that the largest number is in the age of group 26-35y(98), followed by the same number of groups 18-25y(51) and 46-55y(51), followed by 36-45y(20) and 56+(15)
#'