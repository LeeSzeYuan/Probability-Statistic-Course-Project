library(readxl)
UK <- read_excel("C:/Users/Forge-15/Downloads/UK.xlsx")
View(UK)


bedroom.freq <- table(UK[,4]) #barplot
barplot(bedroom.freq,ylim = c(0,10000),col =c("pink","light blue"),main="Bedroom Specification")

floor.freq <- table(UK[,8]) #barplot
barplot(floor.freq,ylim = c(0,10000),col =c("pink","light blue"),main="Floor Specification")

grade <- table(UK[,12]) #pie
labelling <- c("Maybe", "Yes","No")
library(plotrix)
grad <- c(1,3,4,5,6,7,8,9, 10, 11, 12, 13)
pie3D(grade,main = "RGrades",explode=0.5)
legend("topleft",legend<- labelling,fill = rainbow(grad))


condition <- table(UK[,11]) #3d pie
cond <- c(1,2,3,4,5)
pie3D(condition,labels = cond,main = "Condition",explode=0.5)

#sqft living boxplot
boxplot(UK[,6], main = "SQFT living", xlab = "Sqftt",col = "gold",border = "black",horizontal = TRUE, notch = TRUE )


#scatterplot (https://www.statmethods.net/graphs/scatterplot.html)
cor(UK[,6], UK[,3])

#regression line(datacamp)
model <- lm(UK[,3]~UK[,6])
plot(UK[,6], UK[,3], xlim=c(0,2600), ylim=c(0,500))
abline(model)
summary(model)
'''
plot(relationship, ave_time,
     main = "Previous relationship vs Average time willingly to spent(hours)",
     xlab = "The number of previous relationship",
     ylab = "Average time willingly spent(hours) with future partner in a week ", 
     las =1, pch = 19 , col = "purple", frame = FALSE)
'''


#price histogram
price <- c(UK[,3])
print(price)
library(ggplot2)
qplot(UK$price, geom="histogram", bins=40, xlim = c(0, 4e+06), ylim = c(0, 5000), fill=I("blue"), col = I("red"))

curve(dnorm(x, mean=mean(UK$price), sd=sd(UK$price)), add=TRUE, col="red")


#2 sample
#histogram (Price of UK Vs California)

#bell shaped curve?(https://www.statmethods.net/advgraphs/probability.html)