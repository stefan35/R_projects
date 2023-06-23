#' ---
#' title: "Assignment 3"
#' output:
#'    html_document:   
#'      toc: true  
#'      toc_depth: 5 
#'      theme: united
#'      dev: 'svg'
#'      number_sections: false
#' ---
#'
library(readxl)
library(psych)
library(ggplot2)
diet_data <- read_excel('diet.xlsx')
batteries_data <- read_excel('batteries.xlsx')
income_data <- read_excel('income.xlsx')
k <- 2
#'
#' # Income I
#' 
#' The income.xlsx data represents the net annual income of 235 persons. In addition to income, we also have information about
#' 
#' * age: 1: 18-25y, 2: 26-35y, 3: 36-45y, 4: 46-55y, 5: 56+
#' 
#' * education: 1: primary or secondary school without a high school diploma, 
#' 2: high school with graduation, 3: university 1st degree, 4: university 2nd degree, 5: university 3rd degree
#' 
#' * place: 1: smaller village, 2: smaller town, 3: district city, 4: regional city
#' 
#' * sector: 1: services, 2: production, 3: shop
#' 
#' At the significance level α=0.1, test whether the levels of the variables have a statistically significant effect on Income:
#' 
#' * k=2 age and sector
#' 
#' If so, use a post test to determine which levels they are. If not, describe how you would determine this.
#' 
#' **Solution** 
#'
age <- as.factor(income_data$Vek)
sector <- as.factor(income_data$Sektor)
income <- as.numeric(income_data$Prijem)
education <- as.factor(income_data$Vzdelanie)

df_income <- data.frame(age, sector, income, education)

#' $H_{0}$: ∃i,j:μi=μj
#'
#' $H_{1}$: ∃i,j:μi≠μj

ggplot(df_income, aes(x = age, y = income))+
  geom_boxplot(fill=c("#4472c4", "#FF6666", "#FFC733", "#00cc99", "#00cccc"))+
  labs(x= "Category age", y= "Príjem", title = "Comparison of income by age")+
  theme(plot.title = element_text(hjust=0.5))
#' From the graph, we can see that the individual age categories have a similar 
#' distribution of income, interesting is category 3, which does not reach a 
#' significant maximum or minimum, unlike the other categories, another 
#' interesting category is 5, which reaches the lowest maximum
#' 
ggplot(df_income, aes(x = sector, y = income))+
  geom_boxplot(fill=c("#4472c4", "#FF6666", "#FFC733"))+
  labs(x= "Category sector", y= "Príjem", title = "Comparison of income by sector")+
  theme(plot.title = element_text(hjust=0.5))
#' From the graph, we can see that the individual categories of sectors are similar
#' 
#' age
#' 
#' $H_{0}$, data comes from a normal distribution
#' 
#' $H_{1}$, data does not come from a normal distribution
#' 
tapply(df_income$income, df_income$age, shapiro.test)
#' For all categories of age, the p value is greater than 0.1, we cannot reject 
#' $H_{0}$, we cannot say that the data does not come from a normal distribution
#' 
#' $H_{0}$, there is no statistically significant difference between the variance
#' 
#' $H_{1}$, there is a statistically significant difference between the variance
#' 
bartlett.test(df_income$income, df_income$age)
#' At the significance level of 0.1, the p value is larger, we cannot reject 
#' $H_{0}$, we cannot say that there is statistically significant relationship between the variances
#'
#' sector
#'
#' $H_{0}$, data comes from a normal distribution
#' 
#' $H_{1}$, the data does not come from a normal distribution  
#'      
tapply(df_income$income, df_income$sector, shapiro.test)     
#' For all sector categories, the p value is greater than 0.1, we cannot reject 
#' $H_{0}$, we cannot say that the data does not comes from a normal distribution  
#'
#' $H_{0}$, there is no statistically significant difference between the variance
#' 
#' $H_{1}$, there is statistically significant difference between the variance          
#'                  
bartlett.test(df_income$income, df_income$sector)                       
#' At the significance level of 0.1, the p value is larger, we cannot reject $H_{0}$, 
#' we cannot say that there is statistically significant relationship between the variances
#'                                    
an<-aov(income ~ age + sector + age * sector, df_income)
summary(an)
#' At the significance level of 0.1, we did not confirm a statistically significant 
#' difference between age, sector and even their interaction on income, 
#' if we wanted to find out we would do paired T-Tests
#' 
#' 
#' # Income II
#' 
#' There is a statistically significant difference in Income with respect to the level of the variable:
#' 
#' * k=2 education
#' 
#' Test with a suitable test and if there is a difference, use a suitable test to express how 
#' it manifests itself. Also verify the assumptions of using the test.
#' 
#' **Solution** 
#'
ggplot(df_income, aes(x = education, y = income))+
  geom_boxplot(fill=c("#4472c4", "#FF6666", "#FFC733", "#00cc99", "#00cccc"))+
  labs(x= "Category of education", y= "Income", title = "Comparison of income by education")+
  theme(plot.title = element_text(hjust=0.5))
#' From the graph, we can say that category 4 does not have very long whiskers, 
#' unlike category 3, which also has upper and lower outliers, the medians 
#' of categories 4 and 5 are visually similar
#' 
#' The prerequisites for using the test are if we use the Shapiro test to 
#' determine that it comes from a normal distribution and at the same time 
#' using the Bartlett test to determine that there is no statistically significant 
#' difference between the variances
#' 
#' $H_{0}$, data comes from a normal distribution
#' 
#' $H_{1}$, data does not come from a normal distribution
#' 
tapply(df_income$income, df_income$education, shapiro.test)
#' For all levels, the p value is greater than 0.05, we cannot reject $H_{0}$, 
#' we cannot say that the data does not comes from a normal distribution
#' 
#' $H_{0}$, there is no statistically significant difference between the variance
#' 
#' $H_{1}$, there is statistically significant difference between the variance
#' 
bartlett.test(df_income$income, df_income$education)
#'
#' At the significance level of 0.05, the p value is larger, we cannot reject 
#' $H_{0}$, we cannot say that the variance is statistically significantly different
#'
an1 <- aov(income ~ education, df_income)
summary(an1)
#' At the significance level of 0.05, there is a statistically significant 
#' difference between the incomes of people with different educations, 
#' between the attributes income and education
#' 
TukeyHSD(an1)
#' At the significance level of 0.05 we can say that the significant difference 
#' in income is between education 4-1 and 3-1
#'
#'
#'
#' # Diet
#' 
#' The dietitian consultant made a program - diet and fitness plan for 202 patients, 
#' separately for women and separately for men. It was about the age category of 40-50 years. 
#' The weight of the patients at the beginning of the program and after three quarters of a 
#' year of this adjusted lifestyle was recorded. The dietitian assumes that men lose an 
#' average of 15% of their weight and women 13% of their weight. 
#' 
#' The data are in diet.xlsx. We assume normality for each test. 
#' Use an appropriate test to verify if there is a statistically significant difference 
#' before and after the program regardless of gender, test at the given significance level:
#' 
#' * k=2; α=0.05, + whether there is a statistically significant difference in the final weight with respect to gender
#' 
#' **Solution** 
boxplot(diet_data$`Váha pred`, diet_data$`Váha po`, ylab="Weight", names = c('Weight before','Weight after'), col="orange")
#' From the boxplot, we can say that people after the diet weigh less than before the diet
#' 
var.test(diet_data$`Váha pred`,diet_data$`Váha po`)
#' At a significance level of 0.05, the p value is larger, we can say that the data comes from a distribution with similar variance
#' 
t.test(diet_data$`Váha pred`, diet_data$`Váha po`, alternative="two.sided", var.equal=TRUE)
#' At the significance level of 0.05, the p value is smaller, we can say that there is a statistically significant difference between the weight before the diet and after the diet
#' 
m <- diet_data$`Váha po`[diet_data$Pohlavie=='M']
z <- diet_data$`Váha po`[diet_data$Pohlavie=='F']

var.test(m, z)
#' At the significance level of 0.05, the p value is smaller, we can say that 
#' the data for men and women do not come from distributions with similar variance
#' 
t.test(m, z, alternative="two.sided", var.equal=FALSE)
#' At the significance level of 0.05, the p value is smaller, we can say that 
#' there is a statistically significant difference between the resulting weights of men and women
#' 
#' 
#' 
#' # Batteries
#' 
#' The batteries file contains data about battery life 
#' (in days, from four manufacturers A, B, C, D) for devices - geotechnical 
#' monitoring of tunnels and exploration tunnels. The data are from normal distribution. Which of 
#' the 4 manufacturers is the most worthwhile to buy batteries from? Justify 
#' and support with calculations, graphs. Using an appropriate test, verify at 
#' the 0.01 level of significance whether there is a statistically significant 
#' difference in mean battery life from:
#' 
#' * k=2 the two manufacturers for which the median lifetime reaches the highest values
#' 
#' **Solution** 
boxplot(batteries_data$A, batteries_data$B, batteries_data$C, batteries_data$D, names = c('A','B','C','D'), col="yellow")

describe(batteries_data)
#' Through the box graphs and the describe function, we can see that batteries from manufacturer D have the highest endurance
#'
#' Batteries from manufacturer A have the second highest median
#' 
var.test(batteries_data$A, batteries_data$D, conf.level = 1 - 0.01)
#' At a significance level of 0.01, the p value is larger, we can say that the 
#' data come from distributions with similar variance
#' 
t.test(batteries_data$A, batteries_data$D, alternative = 'less', var.equal = TRUE, conf.level = 1 - 0.01)
#' At the significance level of 0.01, the p value is smaller, we can say that 
#' batteries from manufacturer D and A have a statistically significant 
#' difference in endurance
#'
#'