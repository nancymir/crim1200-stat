
library(tidyverse)
library(ggplot2)
library(dplyr)

# 1) DATA
cdph_enforcements <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/total_enforcements_income.csv")

## column names
names(cdph_enforcements)

## column dimensions
dim(cdph_enforcements)

cdph_enforcements %>%
  head(10)

## LOOKUP TABLE FOR NEIGHBORHOOD ZONES
lookup <- c("Albany Park" = 'Northwest',
            "Archer Heights" = 'Southwest',
            "Armour Square" = 'Southwest',
            "Ashburn" = 'Southwest',
            "Auburn Gresham" = 'Near South',
            "Austin" = 'West',
            "Avalon Park" = 'Near South',
            "Avondale" = 'Northwest',
            "Belmont Cragin" = 'Northwest',
            "Beverly" = 'Far South',
            "Bridgeport" = 'Southwest' ,
            "Brighton Park" = 'Southwest',   
            "Burnside" = 'Far South',
            "Calumet Heights" = 'Far South',
            "Chatham" = 'Near South',
            "Chicago Lawn" = 'Southwest',
            "Clearing" = 'Southwest',
            "Douglas" = 'Near South',
            "Dunning" = 'Northwest',
            "East Garfield Park" = 'West',
            "East Side" = 'Far South',
            "Edgewater" = 'North/Central',
            "Edison Park" = 'Northwest',
            "Englewood" = 'Near South',
            "Forest Glen" = 'Northwest',
            "Fuller Park" = 'Near South',
            "Gage Park" = 'Southwest',
            "Garfield Ridge" = 'Southwest',
            "Grand Boulevard" = 'Near South',
            "Greater Grand Crossing" = 'Near South',
            "Hegewisch" = 'Far South',
            "Hermosa" = 'Northwest',
            "Humboldt Park" = 'West',
            "Hyde Park" = 'Near South',
            "Irving Park" = 'Northwest',
            "Jefferson Park" = 'Northwest',
            "Kenwood" = 'Near South',
            "Lake View" = 'North/Central',
            "Lincoln Park" = 'North/Central',
            "Lincoln Square" = 'North/Central',
            "Logan Square" = 'Northwest',
            "Loop" = 'North/Central',
            "Lower West Side" = 'West',
            "McKinley Park" = 'Southwest',
            "Montclare" = 'Northwest',
            "Morgan Park" = 'Far South',
            "Mount Greenwood" = 'Far South',
            "Near North Side" = 'North/Central',
            "Near South Side" = 'North/Central',
            "Near West Side" = 'West',
            "New City" = 'Southwest',
            "North Center" = 'North/Central',
            "North Lawndale" = 'West',
            "North Park" = 'Northwest',
            "Norwood Park" = 'Northwest',
            "O'Hare" = 'Northwest',
            "Oakland" = 'Near South',
            "Portage Park" = 'Northwest',
            "Pullman" = 'Far South',
            "Riverdale" = 'Far South',
            "Rogers Park" = 'North/Central',
            "Roseland" = 'Far South',
            "South Chicago" = 'Near South',
            "South Deering" = 'Far South',
            "South Lawndale" = 'West',
            "South Shore" = "Near South",
            "Uptown" = 'North/Central',
            "Washington Heights" = 'Far South',
            "Washington Park" = 'Near South',
            "West Elsdon" = 'Southwest',
            "West Englewood" = 'Near South',
            "West Garfield Park" = 'West',
            "West Lawn" = 'Southwest',
            "West Pullman" = 'Far South',
            "West Ridge" = 'North/Central',
            "West Town" = 'West',
            "Woodlawn" = 'Near South')

cdph_enforcements$chicago_zone <- unname(lookup[cdph_enforcements$neighborhood])


# VIOLATION DESCRIPTION DATA LONG UPLOAD
long_violation_types <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/long_violation_types.csv")

long_chicago_enforcements <- left_join(cdph_enforcements, long_violation_types, by = "neighborhood")



## INCOME DATA UPLOAD
income <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/Chicago_Per_Capita_Income.csv")

# RENAME INCOME COLUMNS
income <- income %>%
  rename(
    community_area_number = "Community Area Number",
    neighborhood = "COMMUNITY AREA NAME",
    percent_housing_crowded = 'PERCENT OF HOUSING CROWDED',
    percent_households_below_poverty = "PERCENT HOUSEHOLDS BELOW POVERTY",
    percent_aged_over16_unemployed = 'PERCENT AGED 16+ UNEMPLOYED',
    percent_aged_over25_NO_hs_diploma = 'PERCENT AGED 25+ WITHOUT HIGH SCHOOL DIPLOMA',
    percent_aged_under18_or_over64 = 'PERCENT AGED UNDER 18 OR OVER 64',
    per_capita_income = 'PER CAPITA INCOME',
    hardship_income = 'HARDSHIP INDEX'
  )


## REMOVE CHICAGO INCOME
income <- income [-c(78),]

# MERGE INCOME AND ENFORCEMENTS
long_chicago_enforcements <- left_join(long_chicago_enforcements, income, by = "neighborhood")

long_chicago_enforcements <- long_chicago_enforcements[-c(2)]

# INCOME COLUMN TYPO FIX
long_chicago_enforcements <- long_chicago_enforcements %>%
  rename(hardship_index = 'hardship_income')

## fix per capita income duplicate
long_chicago_enforcements <- long_chicago_enforcements[-c(2)]

# rename per capita income column
long_chicago_enforcements <- long_chicago_enforcements %>%
  rename(per_capita_income = 'per_capita_income.y')



# Summary data of violations
summarized_data <- long_chicago_enforcements %>%
  group_by(per_capita_income, chicago_zone, neighborhood) %>%
  summarise(count = n())

## finding mean income of each chicago zone
mean_income_by_zone <- summarized_data %>%
  group_by(chicago_zone) %>%
  summarize(mean_income = mean(per_capita_income))



# SCATTER PLOT OF TOTAL FREQUENCY vs INCOME
ggplot(summarized_data, aes(x = per_capita_income, y = count, fill = chicago_zone)) +
  geom_point() +
  labs(title = "Per Capita Income vs Total Violations Frequency",
       x = "Per Capita Income",
       y = "Violation Frequency")


## H0: There is no relationship between income and number of violations issued
## HA: There is a positive relationship between income and number of violations issued

out <- lm(per_capita_income~count, summarized_data)

# look at diagnostics
par(mfrow=c(2,2))
plot(out)
par(mfrow=c(1,1))

summary(out)

#transform
out.t <- lm(per_capita_income~log(count), summarized_data)
par(mfrow=c(2,2))
plot(out.t)
par(mfrow=c(1,1))


## OMITTED VALUES
summarized_data_omit <- summarized_data[-c(73, 75, 77),]

out.omit <- lm(per_capita_income~count, summarized_data_omit)

# look at diagnostics
par(mfrow=c(2,2))
plot(out.omit)
par(mfrow=c(1,1))

summary(out.omit)

#transform
out.omitt <- lm(per_capita_income~log(count), summarized_data_omit)
par(mfrow=c(2,2))
plot(out.omitt)
par(mfrow=c(1,1))

summary(out.omitt)
