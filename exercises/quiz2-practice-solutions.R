
library(ggplot2)
library(tidyverse)

# Quiz 2


# Normal model

# 1. The patient recovery time from a particular surgical procedure is normally distributed with a mean of 10 days and a standard deviation of 2 days.

# 1.a. What is the z-score for a patient who takes 12 days to recover?

(12-10)/2

# 1.b. What percentage of patients take 20 days or longer to recover?

pnorm(12, mean=10, sd=2, lower.tail = FALSE)

# 1.c. What percentage of patients take between 1 and 5 days to recover?

pnorm(5, mean=10, sd=2, lower.tail = TRUE)-pnorm(1, mean=10, sd=2, lower.tail = TRUE)

# 1.d. How many days does it take for the 20th percentile of the distribution to recover?
qnorm(0.2, mean=10, sd=2)



# Linear regression 

# 2. We are going to study the relationship between number of murders per city and number of robberies, only for cities with population > 1,000,000 using the Uniform Crime Reports from 2017, which are crimes reported to the police, and then compiled by the FBI.

# 2.a. What are some ethical concerns you might have with the data?

# 2.b. Load the data.
dat <- read.csv("data/ucr2017_big_cities.csv")
names(dat)

# Draw a scatterplot of actual_robbery_total vs actual_murder. What does the relationship look like?
dat %>% ggplot(aes(x=actual_murder, y=actual_robbery_total)) + geom_point() 

# Fit a simple linear regression where you regress actual_robbery_total onto actual_murder. 
out <- lm(actual_robbery_total ~ actual_murder, data=dat)

# Before interpreting the coefficients, look at the diagnostic plots. Are the assumptions for fitting the linear model satisfied? 
# 1. Homoscedasticity: 
# check scatterplot and residuals v. fitted.
# 2. Independence between observations:
# can't really check but look for clumping in scatterplot, residuals vs. fitted
# 3. Normality of errors: 
# look at qq plot
# 4. Linear relationship between x and y:
# scatterplot and residuals vs. fitted 

par(mfrow=c(2,2))
plot(out)
par(mfrow=c(1,1))


# What are the outliers? Why do you think they are outliers?
# residuals vs leverage plot. outside cook's distance of 0.5?

dat[3,]
dat[7,]
dat[11,]


# Look at the regression plot on the scatterplot.
dat %>% ggplot(aes(x=actual_murder, y=actual_robbery_total)) + geom_point() + geom_smooth(method='lm', formula= y~x, se=FALSE)


# Regardless of how the diagnostics look, interpret the slope coefficient. What can you conclude about the relationship between murder and robberies? (Ignore p-values or any measure of uncertainty associated with the coefficient since we haven't covered that in class.)

summary(out)

# Each additional murder is associated with 22.78 additional robberies.

