library(tidyverse)
library(ggplot2)

income_data <- read.csv("/Users/nancy/Documents/GitHub/crim1200-stat/income.data.csv")

View(income_data)

# data summary
summary(income_data)

#exploring columns
income_data %>%
  head(10)

income_data %>%
  count(gender)

income_data %>%
  count(education, happiness) %>%
  head(10)

# education x average happiness
income_data %>% 
  group_by(education) %>%
  summarize(average_happiness=mean(happiness))

# gender x average happiness
income_data %>% 
  group_by(gender) %>%
  summarize(average_happiness=mean(happiness))

# education x summary happiness bar plot
income_data %>% 
  group_by(education) %>%
  summarize(average_happiness=mean(happiness)) %>%
  ggplot(aes(x=education, y=average_happiness)) + geom_col()

# gender x summary happiness bar plot
income_data %>% 
  group_by(gender) %>%
  summarize(average_happiness=mean(happiness)) %>%
  ggplot(aes(x=gender, y=average_happiness)) +
  geom_col()

income_data %>%
  ggplot(aes(x=education, y=happiness)) +
  geom_boxplot()

ggplot(income_data, aes(x=income, y=happiness)) + 
  geom_point() # fill gender?

ggplot(income_data, aes(x=education, y=gender)) + 
  geom_bar() 

