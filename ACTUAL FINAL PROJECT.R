# load libraries
library(tidyverse)
library(ggmap)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(mapview)
library(sf)
library(data.table)

## LOAD CDPH ENVIRONMENTAL ENFORCEMENT DATA
cdph_data <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/CDPH_Environmental_Enforcement.csv")


## INSPECT & CLEAN
dim(cdph_data)

names(cdph_data)


cdph_data <- cdph_data %>%
  mutate(date = parse_date_time(`VIOLATION DATE`, orders="mdy"))

cdph_data <- cdph_data %>%
  arrange(date)

cdph_data <- cdph_data %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date))

# FILTERING ENTRIES BY 2012 AND BEYOND
cdph_data <- cdph_data %>%
  filter(year >= 2012)


# EXTRACT NUMERIC CODE FROM 'CODE VIOLATION' COLUMN
cdph_data <- cdph_data %>%
  mutate(numeric_code = str_extract(`CODE VIOLATION`, "\\d+-\\d+-\\d+"))



# CHECKING FREQUENCY OF CODE VIOLATIONS ACROSS THE YEARS
unique(cdph_data$numeric_code)
cdph_data %>%
  count(numeric_code, sort = TRUE) %>%
  head(69)



#NEW DATASET WITH TOP 5 VIOLATIONS
enforcements_top5 <- cdph_data %>%
  filter(numeric_code == "13-32-125" | 
           numeric_code == "11-4-660" | 
           numeric_code == "11-4-760" | 
           numeric_code == "4-108-355" | 
           numeric_code == "11-4-1500")



# LOOKUP TABLE FOR FREQUENT CODE VIOLATIONS
lookup <- c("13-32-125" = 'Construction site cleanliness', 
            "11-4-660" = 'Certificate of operation requiered', 
            "11-4-760" = 'Handling and storage of material susceptible to becoming windborne', 
            "4-108-355" = 'Semiannual surficial cleansing', 
            "11-4-1500" = 'Treatment and disposal of solid or liquid waste')


# adding lookup table to top 5 data set as additional column
enforcements_top5$code_description <- unname(lookup[enforcements_top5$numeric_code])


##NEIGHBORHOOD BOUNDARIES
neighborhoods <- st_read("/Users/nancy/Downloads/Boundaries - Community Areas (current)")

neighborhoods %>%
  ggplot()+
  geom_sf()+
  scale_fill_gradient(low="white", high="black")+
  labs(title="CHICAGO",
       caption = "Data from 2010 Census",
       fill = "CHICAGO
       ") +
  guides(colour=guide_legend(override.aes=list(alpha=1, size=10))) +
  theme_void() +
  theme(plot.title=element_text(size=25, face = "bold", hjust = 0.5),
        plot.subtitle=element_text(size=15, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))

#YESS 

# clean lat/long columns
enforcements_top5 <- enforcements_top5 %>%
  filter(is.na(LATITUDE)== F,
         is.na(LONGITUDE) == F)

# create sf & assign geometry column
enforcements_top5_sf <- st_as_sf(enforcements_top5, wkt = "LOCATION", crs = 4326)

enforcements_top5_sf <- st_set_geometry(enforcements_top5_sf, "LOCATION")


## MAPPING TOP 5 ENFORCEMENTS
enforcements_top5_sf %>%
  mutate(geometry = LOCATION) %>%
  ggplot()+
  geom_sf(data = neighborhoods, fill = 'lightblue') +
  geom_sf(size = 0.5)+
  scale_fill_gradient(low="white", high="black")+
  labs(title="Environmental Enforcements across Chicago from 2012-2023",
       caption = "Data from Chicago Department of Public Health - Environmental Enforcements",
       fill = "CHICAGO") +
  guides(colour=guide_legend(override.aes=list(alpha=1, size=10))) +
  theme_void() +
  theme(plot.title=element_text(size=25, face = "bold", hjust = 0.5),
        plot.subtitle=element_text(size=15, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))


#REVERSE GEOCODE
enforcements_top5_sf <- enforcements_top5_sf%>%
  reverse_geocode(lat = LATITUDE, long = LONGITUDE, method = "osm",
                  address = address_found, full_results = TRUE)


## RENAME COLUMNS
enforcements_top5_sf <- enforcements_top5_sf %>% 
  rename(
    address = 'ADDRESS',
    street_number_from = 'STREET NUMBER FROM',
    street_number_to = 'STREET NUMBER TO',
    direction = 'DIRECTION',
    street_name = 'STREET NAME',
    street_type = 'STREET TYPE',
    docket_no = 'DOCKET NO.',
    ticket_no = 'TICKET NO.',
    respondent = 'RESPONDENT',
    case_type = 'CASE TYPE',
    violation_date = 'VIOLATION DATE',
    code_violation = 'CODE VIOLATION',
    disposition = 'DISPOSITION',
    case_status = 'CASE STATUS',
    fine_amount = 'FINE AMOUNT',
    comment = 'COMMENT',
    data_source = 'DATA SOURCE',
    latitude = 'LATITUDE',
    longitude = 'LONGITUDE',
    location = 'LOCATION',
    code_no = 'numeric_code',
    neighborhood = 'quarter')

# attempting to save data frame as csv
write.csv(enforcements_df, "C:\\Users\\nancy\\Desktop\\screenshots\\enforcements_df.csv", row.names=FALSE)

enforcements_top5_df <- as.data.frame(enforcements_sf)

is.data.frame(enforcements_df)

## dropped na neighborhoods
enforcements_top5_sf <- enforcements_top5_sf %>%
  drop_na(neighborhood)

## summarized number of violations per neighborhood
enforcements_top5_sf %>%
  group_by(neighborhood) %>%
  summarise(violations = n())%>%
  print(n=78)

# COUNTING NUMBER OF NEIHGBORHOODS IN ENFORCEMENT DATA
enforcements_top5_sf %>%
  pull(neighborhood) %>%
  unique %>%
  sort

write.table(enforcements_df, "enforcements_df.txt")
saveRDS(enforcements_df, file="enforcements_df.Rda")
save(enforcements_df, file = "enforcements_df.RData")
dim(enforcements_df)


## UPLOAD INCOME DATA
income <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/Chicago_Per_Capita_Income.csv")

names(income)

dim(income)

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

income %>%
  select('neighborhood') %>%
  arrange('neighborhood') %>%
  print(n = 78)

## REMOVE CHICAGO INCOME
income <- income [-c(78),]

## REDUCE COLUMNS
#neighborhood_income <- income %>%
  #select('COMMUNITY AREA NAME', 'PER CAPITA INCOME')


#neighborhood_income <- neighborhood_income %>%
  #rename(
    #neighborhood = 'COMMUNITY AREA NAME',
    #per_capita_income = 'PER CAPITA INCOME'
  #)


## MERGE NEIGHBORHOOD ENFORCEMENTS AND INCOME
enforcements_income <- merge(enforcements_top5_sf, income, by = "neighborhood", all = TRUE)

names(enforcements_income)

#enforcements_income <- merge(enforcements_sf, neighborhood_income, by = "neighborhood", all = TRUE)

#enforcements_income %>%
  #select(per_capita_income, neighborhood)


## DATASET WITH NEIGHBORHOOD & CORRESPONDING TOTAL NUMBER OF VIOLATIONS
total_enforcements <- enforcements_income %>%
  group_by(neighborhood) %>%
  summarise(violations = n())

## DATASET W/ NEIGHBORHOOD & CORRESPONDING TOTAL NUMBER OF VIOLATION TYPES
violation_types_by_neighborhood <- enforcements_income %>%
  group_by(neighborhood) %>%
  count(code_description) 

## PREVIOUS DATASET, BUT LONG
long_violations_by_neighborhood <- enforcements_income %>%
  group_by(neighborhood) %>%
  select(neighborhood, code_description)

write.csv(long_violations_by_neighborhood, "C:\\Users\\nancy\\Desktop\\long_violation_types.csv", row.names=FALSE)

write.csv(violation_types_by_neighborhood, "C:\\Users\\nancy\\Desktop\\violation_types.csv", row.names=FALSE)

# MERGE TOTAL ENFORCEMENTS AND INCOME
total_enforcements_income <- merge(total_enforcements, neighborhood_income, by = "neighborhood", all = TRUE)

total_enforcements_income %>%
  head(10)

write.csv(total_enforcements_income, "C:\\Users\\nancy\\Desktop\\total_enforcements_income.csv", row.names=FALSE)



