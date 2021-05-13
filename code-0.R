library(dslabs)
#simulate a monte carlo
B <- 10000
N <- 1000
p<- 0.6
x_hat <- replicate(B, {x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)})
mean(x_hat)
sd(x_hat)
#Construct a confidence interval
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
# The proportion of confidence intervals that cover the parameter p (proportion)
N <- 1000
B <- 10000
inside <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  between(p, x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
mean(inside)
#
data("polls_us_election_2016")
library(tidyverse)
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.") 
N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1]/100
# Adding a column that contains the spreads using pipeline
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton / 100 - rawpoll_trump / 100)

