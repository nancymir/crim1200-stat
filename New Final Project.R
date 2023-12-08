# load libraries
library(tidyverse)
library(ggmap)
library(ggplot2)
library(lubridate)
library(stringr)
library(mapview)
library(sf)

#LOAD CDPH ENVIRONMENTAL ENFORCEMENT DATA
dat <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/CDPH_Environmental_Enforcement.csv")


#CLEAN & FILTER
dat <- dat %>%
  mutate(date = parse_date_time(`VIOLATION DATE`, orders="mdy"))

dat <- dat %>%
  arrange(date)

dat <- dat %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date))


# FILTERING ENTRIES BY 2012 AND BEYOND
dat <- dat %>%
  filter(year >= 2012)


# EXTRACT NUMERIC CODE FROM 'CODE VIOLATION' COLUMN
dat <- dat %>%
  mutate(numeric_code = str_extract(`CODE VIOLATION`, "\\d+-\\d+-\\d+"))
dat %>%
  select(numeric_code) %>%
  head(10)


# CHECKING FREQUENCY OF CODE VIOLATIONS ACROSS THE YEARS
unique(dat$numeric_code)

dat %>%
  count(numeric_code, sort = TRUE) %>%
  head(10)


#NEW DATASET WITH TOP 5 VIOLATIONS
enforcements_top5 <- dat %>%
  filter(numeric_code == "13-32-125" | numeric_code == "11-4-660" | numeric_code == "11-4-760" | numeric_code == "4-108-355" | numeric_code == "11-4-1500")


# LOOKUP TABLE FOR FREQUENT CODE VIOLATIONS
lookup <- c("13-32-125" = 'Construction site cleanliness', 
            "11-4-660" = 'Certificate of operation requiered', 
            "11-4-760" = 'Handling and storage of material susceptible to becoming windborne', 
            "4-108-355" = 'Semiannual surficial cleansing', 
            "11-4-1500" = 'Treatment and disposal of solid or liquid waste', 
            "11-4-765" = 'Construction site cleanliness 2', 
            "11-4-2190" = 'Permit and notification requirements for sandblasting, grinding and chemical washing', 
            "11-4-1585" = 'Person responsible for waste removal', "11-4-030" = 'Person responsible shall remove any waste on residence, business, lot, or real estate', 
            "7-28-080" = 'Nuisance in connection with business'
            )


# ADDING LOOKUP TABLE TO TOP 5 DATASET
enforcements_top5$code_description <- unname(lookup[enforcements_top5$numeric_code])

enforcements_top5 %>%
  select(numeric_code, code_description) %>%
  head(10)


# TEST
#enforcements_top5 <- enforcements_top5 %>%
#  filter(is.na(LATITUDE)== F,
     #    is.na(LONGITUDE) == F)

#enforcements_top5 <- st_as_sf(enforcements_top5, coords = c("LONGITUDE", "LATITUDE"),
              #         crs = "NAD83",
               #        remove = F)


# REVERSE GEOCODING DATASET
enforcements_top5 <- enforcements_top5%>%
  reverse_geocode(lat = LATITUDE, long = LONGITUDE, method = "osm",
                  address = address_found, full_results = TRUE)



# MAKING SF DATASET
enforcements_top5 <- enforcements_top5 %>%
  filter(is.na(LATITUDE)== F,
         is.na(LONGITUDE) == F)

enforcements_top5 <- st_as_sf(enforcements_top5, coords = c(18:19))






# CLEANING GEOCODED TOP 5 DATASET
enforcements_top5 <- enforcements_top5 %>%
  drop_na(quarter)

enforcements_top5 %>%
  count(quarter)

enforcements_top5 %>%
  pull(quarter) %>%
  unique %>%
  sort

enforcements <- enforcements_top5 %>% 
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
    neighborhood = 'quarter'
  )



##UPLOAD INCOME DATA
income <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/Per_Capita_Income.csv")

income %>%
  select(`COMMUNITY AREA NAME`) %>%
  arrange(`COMMUNITY AREA NAME`) %>%
  print(n = 78)

## REMOVE CHICAGO INCOME
income <- income [-c(78),]

neighborhood_income <- income %>%
  select('COMMUNITY AREA NAME', 'PER CAPITA INCOME')

neighborhood_income %>%
  rename(
    neighborhood = 'COMMUNITY AREA NAME',
    per_capita_income = 'PER CAPITA INCOME'
  )



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





