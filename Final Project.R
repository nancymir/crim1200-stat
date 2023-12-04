
# load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)

# load data
dat <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/CDPH_Environmental_Enforcement.csv")

view(dat)

dat <- dat %>%
  mutate(date = parse_date_time(`VIOLATION DATE`, orders="mdy"))

dat %>%
  select('VIOLATION DATE', date) %>%
  head(10)

dat %>%
  arrange(date) %>%
  select(date)
  head(10)
  
  