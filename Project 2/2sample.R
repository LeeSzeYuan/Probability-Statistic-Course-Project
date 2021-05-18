#2 sample
library(readxl)
USA <- read_excel("USA.xlsx")
View(USA)

#Two-way Contingency Table
dataFrame <- with(USA, table(grade, condition))

alpha <- 0.05

chisq.test(dataFrame, correct=FALSE)
x2.alpha <- qchisq(alpha, df=44, lower.tail=FALSE)


#Two Means, independent samples,unknown & Unequal ??2
California <- read_excel("California.xlsx")
View(California)

n1 <- nrow(USA)
xbar1 <- mean(USA$price)
s1 <- sd(USA$price)

n2 <- nrow(California)
xbar2 <- mean(California$median_house_value)
s2 <- sd(California$median_house_value)

t0 = (xbar1-xbar2-0)/(sqrt((s1^2/n1)+(s2^2/n2)))

v = ((s1^2/n1)+(s2^2/n2))^2/((((s1^2/n1)^2)/(n1-1))+(((s2^2/n2)^2)/(n2-1)))

alpha = 0.05
t.alpha = qt(alpha/2, floor(v))