
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



## VIOLATION DESCRIPTION DATA UPLOAD
violation_types <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/violation_types.csv")

names(violation_types)

# transform to wide data
violation_types_df <- violation_types %>%
  pivot_wider(names_from = code_description, values_from = n)

violation_types_df %>%
  head(10)

chicago_enforcements <- left_join(cdph_enforcements, violation_types_df, by = "neighborhood")


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


# 1) EDA Single Variables

# 1a) per_capita_income

#quantitative
hist(long_chicago_enforcements$per_capita_income)

#visual
hist(long_chicago_enforcements$per_capita_income)

long_chicago_enforcements %>% ggplot(aes(x=factor(per_capita_income))) + geom_histogram()

# 1b) hardship index

#quantitative
hist(long_chicago_enforcements$hardship_index)

barplot(table(long_chicago_enforcements$hardship_index))

long_chicago_enforcements %>% ggplot(aes(x=factor(hardship_index))) + geom_histogram()

#categorical
table(long_chicago_enforcements$neighborhood)

#visual
barplot(table(long_chicago_enforcements$neighborhood))

tab1 <- table(long_chicago_enforcements$neighborhood)

tab1 <- as.data.frame(tab1) %>% rename(neighborhood=Var1)

ggplot(data = tab1, aes(x=neighborhood, y=Freq)) + geom_bar(stat = "identity")


#CDPH ENFORCEMENTS

# scatter plot
ggplot(cdph_enforcements, aes(x = per_capita_income, y = violations, fill=chicago_zone)) +
  geom_point() +
  labs(title = "Scatter Plot of Violations vs. Per Capita Income", x = "Per Capita Income", y = "Violations")

# bar plot
ggplot(total_enforcements_income, aes(x = neighborhood, y = violations)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Violations by Neighborhood", x = "Neighborhood", y = "Violations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# bar plot
ggplot(cdph_enforcements, aes(x = chicago_zone, y = violations)) +
  geom_bar(stat = "identity") +
  labs(title = "Chicago Zone and Their Number of Violations", x = "Chicago Zone", y = "Number of Violations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


long_chicago_enforcements %>%
  group_by(code_description) %>%
  summarise(per_capita_income = n())

# making construction / income subset
construction_income <- long_chicago_enforcements %>%
  filter(code_description == 'Construction site cleanliness') %>%
  select(code_description, per_capita_income)


## Scatter plot of the frequency of construction cleanliness violations by their appropriate income
ggplot(construction_income, aes(x = per_capita_income)) +
  geom_point(aes(y = ..count..), stat = "count") +
  labs(title = "Construction Cleanliness Violations Frequency vs Per Capita Income",
       x = "Per Capita Income",
       y = "Frequency of code_description")

# disposal / income subset
disposal_income <- long_chicago_enforcements %>%
  filter(code_description == 'Treatment and disposal of solid or liquid waste') %>%
  select(code_description, per_capita_income)

## Scatter plot of the frequency of disposal violations by their appropriate income
ggplot(disposal_income, aes(x = per_capita_income)) +
  geom_point(aes(y = ..count..), stat = "count") +
  labs(title = "Disposal of Waste Violations Frequency vs Per Capita Income",
       x = "Per Capita Income",
       y = "Frequency of code_description")


## Scatter plot of the frequency of total violations by income
ggplot(long_chicago_enforcements, aes(x = per_capita_income)) +
  geom_point(aes(y = ..count..), stat = "count") +
  labs(title = "Per Capita Income vs Total Violations Frequency",
       x = "Per Capita Income",
       y = "Violation Frequency")



# Summarize the data to get counts for each combination of per_capita_income and chicago_zone
summarized_data <- long_chicago_enforcements %>%
  group_by(per_capita_income, chicago_zone) %>%
  summarise(count = n())

# Create a scatter plot
ggplot(summarized_data, aes(x = per_capita_income, y = count, fill = chicago_zone)) +
  geom_point() +
  labs(title = "Per Capita Income vs Total Violations Frequency",
       x = "Per Capita Income",
       y = "Violation Frequency")



# fill colors not responding
ggplot(summarized_data, aes(x = per_capita_income, y = count, fill = chicago_zone)) +
  geom_point() +
  scale_fill_manual(values = c("Far South" = "red", "Near South" = "orange", "North/Central" = "pink", "Northwest" = "blue", "Southwest" = 'purple', "West" = 'green')) +
  labs(title = "Total Violations Frequency vs Per Capita Income",
       x = "Per Capita Income",
       y = "Frequency of code_description")


plot(long_chicago_enforcements$per_capita_income)

