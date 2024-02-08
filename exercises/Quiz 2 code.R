
library(tidyverse)
library(ggplot2)

#Q1
(732 - 527) / 112
pnorm(0.03, lower.tail=FALSE)

#Q2


#Q3
qnorm(0.99, mean=527, sd=112)

#Q4
pnorm(732, mean=527, sd=112, lower.tail = TRUE) - pnorm(700, mean=527, sd=112, lower.tail = TRUE)

#Q5

drug_data <-read.csv("/Users/nancy/Documents/GitHub/crim1200-stat/dat.nsduh.small.1.1.csv")

view(drug_data)

#Q6

plot(drug_data$mjage, drug_data$ciage)

