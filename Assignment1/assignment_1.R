#' ---
#' title: "Assignment 1"
#' output:
#'    html_document:   
#'      toc: true  
#'      toc_depth: 5 
#'      theme: united
#'      dev: 'svg'
#'      number_sections: false
#' ---
#'
#' # Example 1
#' 
#' The manufacturer guarantees that the germination rate of the seeds of the special pea variety is 80%.
#' The gardener bought 20 pea seeds. Calculate these probabilities
#' 
#' a. all seeds will germinate
#' 
#' b. no more than 6 seeds will germinate
#' 
#' c. germinate at least 6-1 seeds
#'
#' d. calculate the mean and variance
#'
#' e. construct a probability distribution graph
#' 
#' **Solution a.** 
dbinom(20, 20, 0.8)
#' **Solution b.** 
pbinom(6, 20, 0.8)
#' **Solution c.** 
pbinom(4, 20, 0.8, lower.tail = F)
#' **Solution d.** 
20*0.8 #median
20*0.8*0.2 #dispersion
#' **Solution e.** 
xB <- c(0:20)
probB <- dbinom(xB, 20, 0.8)
tabB  <- data.frame(hodnota=xB, prob=probB)
barplot(tabB$prob,main="Graph of the Binomial Distribution",xlab="Value",ylab="Probability", names.arg = xB, col="blue")
#'
#' 
#' # Example 2
#' 
#' Solve the previous example using the hypergeometric distribution if we know
#' that the gardener was picking from 400 pea seeds. Compare the probability distribution graphs.
#' 
#' a. All seeds will germinate
#'
#' b. No more than 6 seeds will germinate
#'
#' c. Germinate at least 6-1 seeds
#'
#' d. Calculate the mean and variance
#'
#' e. Construct a probability distribution graph
#' 
#' **Solution a.** 
dhyper(20, 320, 80, 20)
#' **Solution b.** 
phyper(6, 320, 80, 20)
#' **Solution c.** 
phyper(4, 320, 80, 20, lower.tail = F)
#' **Solution d.** 
20*320/400 #median
20*320/400*(1-320/400)*(400-20)/(400-1) #dispersion
#' **Solution e.** 
xH <- c(0:20)
probH <- dhyper(xH, 320, 80, 20)
tabH <- data.frame(hodnota=xH, prob=probH)
barplot(tabH$prob,main="Graph of the Hypergeometric distribution",xlab="Value",ylab="Probability",names.arg = xH, col="orange")

# Both distribution graphs from example 1 and example 2 are very similar in the acquired values.
#'
#' # Example 3
#' 
#' The website of the online store is visited during the monitored period within one
#' hours on average 30 interested parties. Consider a time interval of 20 minutes. What is the probability that
#' 
#' a. 15 interested parties visit the page
#' b. the page will be visited by at least 6 interested parties
#' c. what is the most likely number of visits in 10 minutes?
#' d. construct a probability distribution graph for the first 15 values of the random variable (we consider a time interval of 20 minutes).
#' e. generate 5 random distribution numbers (for an interval of 20 minutes).
#' 
#' **Solution a.** 
dpois(15, (30/60)*20)
#' **Solution b.** 
ppois(5, (30/60)*20, lower.tail = F)
#' **Solution c.** 
prob <- dpois(c(0:15),(30/60)*10)
prob_val <- which(prob %in% max(prob))
prob_val-1
#' **Solution d.** 
xP <- c(0:15)
probP <- dpois(xP, (30/60)*20)
tabP <- data.frame(hodnota=xP, prob=probP)
barplot(tabP$prob,main="Graf of the Poisson distribution",xlab="Value",ylab="Probability",names.arg = xP, col="yellow")
#' **Solution e.** 
rpois(5, (30/60)*20)
#' 
#' # Example 4
#' 
#' The duration of the exam follows a normal distribution with a mean value of 240 minutes and an authoritative
#' deviation of 10 minutes. Calculate these tasks
#' 
#' a. what % of students complete the test within 220 minutes?
#' b. what % of students finish the test in the time interval of 210-230 minutes?
#' c. what % of students will need at least 235 minutes to complete the test?
#' d. determine the optimal time for finishing the exam so that at least 75% of the students can submit the assignment at that time.
#' 
#' **Solution a.** 
pnorm(220, mean = 240, sd = 10) * 100
#' **Solution b.** 
(pnorm(230, mean = 240, sd = 10)-pnorm(210, mean = 240, sd = 10)) * 100
#' **Solution c.** 
pnorm(234, mean=240, sd=10, lower.tail = F) * 100
#' **Solution d.** 
qnorm(0.75, mean=240, sd=10)
#' 
#' # Example 5
#' 
#' The lifetime of a PC component has an exponential probability distribution with a mean of 2000 days.
#' Calculate these probabilities
#' 
#' a. the component will be functional for at least 6\*100 days
#' b. that the component will fail within its average lifetime
#' c. determine the maximum warranty period that its manufacturer is willing to provide if it allows only 5% of complaints.
#' 
#' **Solution a.** 
pexp(599, rate=1/2000, lower.tail = F)
#' **Solution b.** 
pexp(2000, rate=1/2000)
#' **Solution c.** 
qexp(0.05, rate=1/2000)