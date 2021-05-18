library(readxl)
USA <- read_excel("USA.xlsx")
View(USA)

#Bedrooms
n <- nrow(USA)
x1 <- c(USA$bedrooms)
y <- c(USA$price)

r <- cor(x1,y)
plot(x1,y, xlim=c(0,max(x1)),
     ylim=c(0,max(y)), xlab="Bedrooms", ylab="Price", col="blue")

cor.test(x1, y)

t<-r/sqrt((1-r^2)/(n-2))
talpha <- qt(0.025, n-2) #2 sample
pval = 2*pt(t, df=n-2, lower.tail = FALSE)


#Floors
x2 <- c(USA$floors)

r<-cor(x2, y)
plot(x2,y, xlim=c(0,max(x2)),
     ylim=c(0,max(y)), xlab="Floors", ylab="Price", col="red")

cor.test(x2, y)

t<-r/sqrt((1-r^2)/(n-2))
talpha <- qt(0.025, n-2) #2 sample
pval = 2*pt(t, df=n-2, lower.tail = FALSE)