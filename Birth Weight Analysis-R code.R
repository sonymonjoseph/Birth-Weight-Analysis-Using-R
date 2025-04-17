#Key statistics
#bwt
mean(Birthweights$bwt)
median(Birthweights$bwt)
sd(Birthweights$bwt)
min(Birthweights$bwt)
max(Birthweights$bwt)
#gestation
mean(Birthweights$gestation)
median(Birthweights$gestation)
sd(Birthweights$gestation)
min(Birthweights$gestation)
max(Birthweights$gestation)
#age
mean(Birthweights$age)
median(Birthweights$age)
sd(Birthweights$age)
min(Birthweights$age)
max(Birthweights$age)
#height
mean(Birthweights$height)
median(Birthweights$height)
sd(Birthweights$height)
min(Birthweights$height)
max(Birthweights$height)
#weight
mean(Birthweights$weight)
median(Birthweights$weight)
sd(Birthweights$weight)
min(Birthweights$weight)
max(Birthweights$weight)
#region
table(Birthweights$region)
#smoker
table(Birthweights$smoke)

#Histogram
x = Birthweights$bwt

h = hist(x, breaks = 20, col = "red", 
         xlab = "Birth Weight(grams)", 
         main = "Histogram of Birth Weight ")

xfit = seq(min(x), max(x), length = 60)
yfit = dnorm(xfit, mean = mean(x), sd = sd(x))
yfit = yfit * diff(h$mids[1:2]) * length(x)

lines(xfit, yfit, col = "blue", lwd = 2)

#t test

t.test(Birthweights$bwt,mu=3400)

#Simple Linear Regression



#Simple Linear Regression Model
simple.fit<-lm(bwt~smoke, data = Birthweights)
LinearModel<-simple.fit
summary(LinearModel)
#mean of the dependent variable "bwt"
mean(Birthweights$bwt)
#Correlation
cor(Birthweights$smoke,Birthweights$bwt)

#Multiple Regression Model

full.model <- lm(bwt ~., data = Birthweights)
summary(full.model)




