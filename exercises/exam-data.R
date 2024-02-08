
library(tidyverse)
library(ggplot2)

prison_data <- read.csv("/Users/nancy/Documents/GitHub/crim1200-stat/crime.dat.csv")

view(prison_data)

#select crimtype and show first 15 columns
prison_data %>%
  select(crimetype) %>%
  head(15)

#graph crimetype by count
prison_data %>%
ggplot(aes(x=crimetype)) +
  geom_bar(binwidth=1)

#summary data
prison_data %>%
  summary()

#counts of crimtetype
prison_data %>%
  count(crimetype)

# % of total
181/300
99/300
20/300

#graph of mentalhealthscore
prison_data %>% 
  ggplot(aes(x=mentalhealthscore)) +
  geom_histogram()

prison_data %>% 
  ggplot(aes(x=mentalhealthscore)) +
  geom_boxplot()

# comparing gender and average mentalhealthscore in bar graph
prison_data %>%
  group_by(gender) %>%
  summarize(average_mentalhealthscore=mean(mentalhealthscore)) %>%
  ggplot(aes(x=gender, y=average_mentalhealthscore)) + geom_col()

# comparing gender and mentalhealthscore in boxplot
prison_data %>%
  ggplot(aes(x=gender, y=mentalhealthscore)) +
  geom_boxplot()

#comparing gender and mentalhealthscore by numbers
prison_data %>%
  select(gender="female", mentalhealthscore)

#comparing age and mentalhealthscore in bar graph
prison_data %>%
  group_by(age) %>%
  summarize(average_mentalhealthscore=mean(mentalhealthscore)) %>%
  ggplot(aes(x=age, y=average_mentalhealthscore)) + geom_col()

# comparing gender and mentalhealthscore in boxplot
prison_data %>%
  ggplot(aes(x=age, y=mentalhealthscore)) +
  geom_boxplot()

prison_data %>%
  group_by(gender,mentalhealthscore) %>%
  summarize(mean)
