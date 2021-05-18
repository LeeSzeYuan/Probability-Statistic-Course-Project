library(readxl)
USA <- read_excel("USA.xlsx")
View(USA)

y <- c(USA$price)

#sqft_living--------------------------------------------------
x1 <- c(USA$sqft_living)
model1 <- lm(y~x1)

plot(x1,y, xlim=c(0,max(x1)),
     ylim=c(0,max(y)), xlab="srft_living", ylab="Price", col="blue")

abline(model1)

summary(model1)
r2 <- cor(x1, y)^2

#Grade--------------------------------------------------------
x2 <- c(USA$grade)
model2 <- lm(y~x2)

plot(x2,y, xlim=c(0,max(x2)),
     ylim=c(0,max(y)), xlab="House Grades", ylab="Price", col="red")

abline(model2)

summary(model2)
r2 <- cor(x2, y)^2

#Condition-----------------------------------------------------
x3 <- c(USA$condition)
model3 <- lm(y~x3)

plot(x3,y, xlim=c(0,max(x3)),
     ylim=c(0,max(y)), xlab="House Conditions", ylab="Price", col="blue")

abline(model3)

summary(model3)
r2 <- cor(x3, y)^2