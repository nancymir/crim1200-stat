
library(tidyverse)
library(ggplot2)

# 1) DATA
cdph_enforcements <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/total_enforcements_income.csv")

## column names
names(cdph_enforcements)

## column dimensions
dim(cdph_enforcements)

cdph_enforcements %>%
  head(10)


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
            "Englewood" = 'Near South Side',
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
            "Near South Side" = 'Near/Central',
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


violation_types <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/violation_types.csv")

names(violation_types)

violation_types_df <- violation_types %>%
  pivot_wider(names_from = code_description, values_from = n)

table(cdph_enforcements$violations)

hist(cdph_enforcements$violations)





# GRAPHS

# # Scatter plot
ggplot(total_enforcements_income, aes(x = per_capita_income, y = violations)) +
  geom_point() +
  labs(title = "Scatter Plot of Violations vs. Per Capita Income", x = "Per Capita Income", y = "Violations")

# bar plot
ggplot(total_enforcements_income, aes(x = neighborhood, y = violations)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Violations by Neighborhood", x = "Neighborhood", y = "Violations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))