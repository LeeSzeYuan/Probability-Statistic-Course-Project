#Confidence Interval
library(readxl)
USA <- read_excel("USA.xlsx")
View(USA)

pop.mean <- 385000

n <- nrow(USA)

xbar <- mean(USA$price)
std <- sd(USA$price)


#Confidence Interval 95%
t <- qt(0.95, n-1)
se <- (std/sqrt(n))
errmargin <- (t)*(se)


interval <- xbar +c(-errmargin, errmargin)


#Hypotheses Testing on Mean with Variance Unknown
alpha <- 0.05
z.alpha = qnorm(1-(alpha/2))

z <- (xbar - pop.mean)/(std/sqrt(n))
pval = 2*pnorm(z) 

c(-z.alpha, z.alpha)


#Chi-Square test on Variance
sigma <- 400000
alpha <- 0.05
df <- n-1

x2.alpha = qchisq(1-alpha, df=n-1)
x2 <- ((n-1)*std^2)/(sigma^2)