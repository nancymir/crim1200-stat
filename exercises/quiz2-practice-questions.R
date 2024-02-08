
# Quiz 2


# Normal model

# 1. The patient recovery time from a particular surgical procedure is normally distributed with a mean of 10 days and a standard deviation of 2 days.

# 1.a. What is the z-score for a patient who takes 12 days to recover?

(12-10)/2

# 1.b. What percentage of patients take 20 days or longer to recover?
pnorm(20, mean=10, sd=2, lower.tail=FALSE) #to the right

# 1.c. What percentage of patients take between 1 and 5 days to recover?
pnorm(5, mean=10, sd=2) - pnorm(1, mean=10, sd=2)
# 1.d. How many days does it take for the 20th percentile of the distribution to recover?
qnorm(0.20, mean=10, sd=2,)



# Linear regression 

# 2. We are going to study the relationship between number of murders per city and number of robberies, only for cities with population > 1,000,000 using the Uniform Crime Reports from 2017, which are crimes reported to the police, and then compiled by the FBI.

# 2.a. What are some ethical concerns you might have with the data?

# 2.b. Load the data.
library(tidyverse)
library(ggplot2)

ucr_data <- read.csv("/Users/nancy/Documents/GitHub/crim1200-stat/ucr2017_big_cities.csv")

view(ucr_data)

# 2.c. Draw a scatterplot of actual_robbery_total vs actual_murder. What does the relationship look like?

ggplot(ucr_data, aes(x=actual_robbery_total, y=actual_murder)) + geom_point() # the relationship looks curved and fanning out


# 2.d. Fit a simple linear regression where you regress actual_robbery_total onto actual_murder. 
# lm(y~out, data)
#summary(out)
#plot(out)

lm.output <- lm(actual_robbery_total ~ actual_murder, ucr_data)
summary(lm.output)

par(mfrow=c(1,1))
plot(lm.output)

#residuals vs fitted plot should look boring 

# Before interpreting the coefficients, look at the diagnostic plots. Are the assumptions for fitting the linear model satisfied? 
# A. Homoscedasticity: 
# B. Independence between observations:
# C. Normality of errors: 
# D. Linear relationship between x and y:


# 2.e. What are the outliers? Why do you think they are outliers?

ucr_data[7,]
ucr_data[11,]

# 2.f. Look at the regression plot on the scatterplot.
ucr_data %>% ggplot(aes(x=actual_murder, y=actual_robbery_total)) + geom_point() + geom_smooth(method='lm', formula= y~x, se=FALSE)

# 2.g. Regardless of how the diagnostics look, interpret the slope coefficient. What can you conclude about the relationship between murder and robberies? (Ignore p-values or any measure of uncertainty associated with the coefficient since we haven't covered that in class.)
#look at slope estimated, under intercept

lm.output <- lm(actual_robbery_total ~ actual_murder, ucr_data)
summary(lm.output)

#Each additional murder is associated with 22.78 additional robberies



