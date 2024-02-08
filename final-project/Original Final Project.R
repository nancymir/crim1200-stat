
# load libraries
library(tidyverse)
library(tidygeocoder)
library(ggmap)
library(ggplot2)
library(lubridate)
library(stringr)
library(mapview)
library(sf)


#LOAD CDPH ENVIRONMENTAL ENFORCEMENT DATA
dat <- read_csv("/Users/nancy/Documents/GitHub/crim1200-stat/CDPH_Environmental_Enforcement.csv")

dat %>%
  colnames()

view(dat)

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

dat %>%
  names()

dat %>%
  count(year)

# EXTRACT NUMERIC CODE FROM 'CODE VIOLATION' COLUMN
dat <- dat %>%
  mutate(numeric_code = str_extract(`CODE VIOLATION`, "\\d+-\\d+-\\d+"))
dat %>%
  select(numeric_code) %>%
  head(10)

view(dat)

# cannot separate character strings from 'CODE VIOLATION'
#newer_dat <- dat %>%
# mutate(String_Part = str_extract(`CODE VIOLATION`, "\\Full Description:+"))

#newer_dat %>%
# select(String_Part) %>%
# head(10)

# CHECKING FREQUENCY OF CODE VIOLATIONS ACROSS THE YEARS
unique(dat$numeric_code)

dat %>%
  count(numeric_code, sort = TRUE) %>%
  head(10)

dat %>%
  filter(numeric_code == '13-32-125')%>%
  select('CODE VIOLATION')

dat %>%
  filter(numeric_code == '11-4-660')%>%
  select('CODE VIOLATION')

dat %>%
  filter(numeric_code == '11-4-760')%>%
  select('CODE VIOLATION')

dat %>%
  filter(numeric_code == '11-4-765')%>%
  select('CODE VIOLATION')

#NEW DATASET WITH TOP 10 VIOLATIONS
top10 <- dat %>%
  filter(numeric_code == "13-32-125" | numeric_code == "11-4-660" | numeric_code == "11-4-760" | numeric_code == "4-108-355" | numeric_code == "11-4-1500" | numeric_code == "11-4-765" | numeric_code == "11-4-2190" | numeric_code == "11-4-1585" | numeric_code =="11-4-030"| numeric_code =="7-28-080")
view(top10)

#NEW DATASET WITH TOP 5 VIOLATIONS
top5 <- dat %>%
  filter(numeric_code == "13-32-125" | numeric_code == "11-4-660" | numeric_code == "11-4-760" | numeric_code == "4-108-355" | numeric_code == "11-4-1500")
view(top5)

# LOOKUP TABLE FOR FREQUENT CODE VIOLATIONS
lookup <- c("13-32-125" = 'Construction site cleanliness', "11-4-660" = 'Certificate of operation requiered', "11-4-760" = 'Handling and storage of material susceptible to becoming windborne', "4-108-355" = 'Semiannual surficial cleansing', "11-4-1500" = 'Treatment and disposal of solid or liquid waste', "11-4-765" = 'Construction site cleanliness 2', "11-4-2190" = 'Permit and notification requirements for sandblasting, grinding and chemical washing', "11-4-1585" = 'Person responsible for waste removal', "11-4-030" = 'Person responsible shall remove any waste on residence, business, lot, or real estate', "7-28-080" = 'Nuisance in connection with business')

# adding lookup table to top 10 dataset
top10$code_description <- unname(lookup[top10$numeric_code])

top10 %>%
  select(numeric_code, code_description) %>%
  head(10)

# adding lookup table to top 5 dataset
top5$code_description <- unname(lookup[top5$numeric_code])

top5 %>%
  select(numeric_code, code_description) %>%
  head(10)

view(top5)

## CPDH ENFORCEMENTS BY YEAR
cpdh_enforcements2012 <- top5%>%
  filter(year == 2012)
cpdh_enforcements2013 <- top5%>%
  filter(year == 2013)
cpdh_enforcements2014 <- top5%>%
  filter(year == 2014)
cpdh_enforcements2015 <- top5%>%
  filter(year == 2015)
cpdh_enforcements2016 <- top5%>%
  filter(year == 2016)
cpdh_enforcements2017 <- top5%>%
  filter(year == 2017)
cpdh_enforcements2018 <- top5%>%
  filter(year == 2018)
cpdh_enforcements2019 <- top5%>%
  filter(year == 2019)
cpdh_enforcements2020 <- top5%>%
  filter(year == 2020)
cpdh_enforcements2021 <- top5%>%
  filter(year == 2021)
cpdh_enforcements2022 <- top5%>%
  filter(year == 2022)
cpdh_enforcements2023 <- top5%>%
  filter(year == 2023)



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

top10 <- top10 %>%
  filter(is.na(LATITUDE)== F,
         is.na(LONGITUDE) == F)

top10 <- st_as_sf(top10, coords = c(18:19))

top10 %>%
  mutate(geometry = LOCATION) %>%
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


#MAPPING EE DATA
top5 <- top5 %>%
  filter(!is.na('LATITUDE') & !is.na('LONGITUDE'),
         year == 2012|year == 2013|year == 2014|year == 2015|year == 2016|year == 2017|year == 2018|year == 2019|year == 2020|year == 2021|year == 2022|year == 2023,
         numeric_code=="13-32-125",
         'LATITUDE'>37) %>%
  st_as_sf(
    coords = c('LONGITUDE', 'LATITUDE'),
    crs = 4326
  )

top5_summary <- top5 %>%
  group_by(`Community Area`)%>%
  summarise(numeric_code = n())%>%
  st_drop_geometry()

