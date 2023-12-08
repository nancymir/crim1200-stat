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



# CHECKING FREQUENCY OF CODE VIOLATIONS ACROSS THE YEARS
unique(dat$numeric_code)



#NEW DATASET WITH TOP 5 VIOLATIONS
enforcements <- dat %>%
  filter(numeric_code == "13-32-125" | numeric_code == "11-4-660" | numeric_code == "11-4-760" | numeric_code == "4-108-355" | numeric_code == "11-4-1500")



# LOOKUP TABLE FOR FREQUENT CODE VIOLATIONS
lookup <- c("13-32-125" = 'Construction site cleanliness', 
            "11-4-660" = 'Certificate of operation requiered', 
            "11-4-760" = 'Handling and storage of material susceptible to becoming windborne', 
            "4-108-355" = 'Semiannual surficial cleansing', 
            "11-4-1500" = 'Treatment and disposal of solid or liquid waste')


# adding lookup table to top 5 dataset
enforcements$code_description <- unname(lookup[enforcements$numeric_code])


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

enforcements <- enforcements %>%
  filter(is.na(LATITUDE)== F,
         is.na(LONGITUDE) == F)

enforcements_sf <- st_as_sf(enforcements, wkt = "LOCATION", crs = 4326)

enforcements_sf <- st_set_geometry(enforcements_sf, "LOCATION")


#MAP
enforcements_sf %>%
  mutate(geometry = LOCATION) %>%
  ggplot()+
  geom_sf(data = neighborhoods, fill = 'lavender') +
  geom_sf(size = 0.5)+
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


#REVERSE GEOCODE
enforcements_sf <- enforcements_sf%>%
  reverse_geocode(lat = LATITUDE, long = LONGITUDE, method = "osm",
                  address = address_found, full_results = TRUE)


#RENAME COLUMNS
enforcements_sf <- enforcements_sf %>% 
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

## DROPPING NA NEIGHBORHOODS
enforcements_sf <- enforcements_sf %>%
  drop_na(neighborhood)


enforcements_sf %>%
  group_by(neighborhood) %>%
  summarise(violations = n())%>%
  print(n=78)

#COUNTING NUMBER OF NEIHGBORHOODS IN ENFORCEMENT DATA
enforcements_sf %>%
  pull(neighborhood) %>%
  unique %>%
  sort


##UPLOAD INCOME DATA
income <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/Per_Capita_Income.csv")