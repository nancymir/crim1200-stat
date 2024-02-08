
library(tidyverse)
library(ggplot2)

# 1. Data

# a. Load the data and preview it. How many individuals' responses were included?

dat <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/data.citizen.complaints.csv") 

view(dat)

nrow(dat)

# 2. EDA single variable: Do EDA, visual and quantitative, and write down in words what are some surprising features of each variable.

# a. Describe citizen.complaints visually and quantitatively

dat %>% ggplot(aes(x=citizen.complaints))+geom_histogram()

summary(dat$citizen.complaints)

# b. Describe reports.filed.by.officer visually and quantitatively

dat %>% ggplot(aes(x=reports.filed.by.officer))+geom_histogram()

summary(dat$reports.filed.by.officer)

# c. Describe bwc visually and quantitatively

dat %>% ggplot(aes(x=bwc))+geom_bar()

table(dat$bwc)


# 3. Comparing a quantitative variable and a categorical variable.

# a. EDA two variables: Describe the relationship between the two variables visually and quantitatively. (Remember to include categorical variables in ggplot as "factor(variable)" instead of just "variable".)
# citizen.complaints and bwc

dat %>% ggplot(aes(x=factor(bwc), y=citizen.complaints)) + geom_boxplot()

with(dat, by(citizen.complaints, bwc, summary))

# b. Statistical test: Run a t-test to test whether there are more citizen complaints when police officers wear a camera than when they do not. What are the null and alternative hypotheses? Is the p-value < 0.05? What can you conclude from this test?

# c. Does this mean that if we give all police officers a body-worn camera, they will have a better relationship with the community?

# d. Repeat the last three points comparing reports.filed.by.officer and bwc. What can you conclude here?



# 4. Comparing two quantitative variables.

# a. EDA two variables: Describe the relationship between citizen.complaints (y) and reports.filed.by.officer (x).

dat %>% ggplot(aes(x=reports.filed.by.officer, y=citizen.complaints)) + geom_point()

# b. Fit a linear model regressing citizen.complaints (y) onto reports.filed.by.officer (x). Are the assumptions of a linear model satisfied? If so, move on to the next problem. If not, try transforming the data.
out <- lm(citizen.complaints ~ reports.filed.by.officer, data=dat)


par(mfrow=c(2,2))
plot(out)

# c. Interpret the slope coefficient. What can you say about the relationship between citizen.complaints (y) and reports.filed.by.officer (x)? Do we see more citizen complaints for officers that produce more reports against citizens? 

summary(out)

# d. What does the confidence interval on the slope coefficient tell us?


