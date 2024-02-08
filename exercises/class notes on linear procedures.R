
#procedure
?cars

par(mfrow=c(1,1))

hist(cars$speed)
hist(cars$dist)

#scatterplot
plot(cars$speed, cars$dist)

#linear model
lm.output <- lm(formula = dist ~ speed, data = cars)
lm.output <- lm(dist ~ speed, cars)

summary(lm.output)

lm.output$residuals
lm.output$fitted.values

plot(lm.output$residuals, cars$speed)
