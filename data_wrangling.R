pacman::p_load("tidyverse", "googlesheets4", "googledrive", "lubridate", "corrr")

library(usethis)

use_git()
use_github()

drive_sheet <- "https://docs.google.com/spreadsheets/d/1Fx1wFh_z_FCzztZuScGicivwRmyDgR1p35bcu0yRsrI/edit#gid=13038978"

flights_sheet <- read_sheet(drive_sheet, sheet = "All Regions Requests Multi Pati")
saveRDS(flights_sheet, "flights.rds")

socialMedia_sheet <- read_sheet(drive_sheet, sheet = "Social Media Data")
saveRDS(socialMedia_sheet, "socialMedia.rds")



### lining up weeks.  Fligts exact date.  SM week date (beginning or end)

### focus SM: number of posts, platforms, what is score (huge range), metric (ER, Fans, Posts)

### note: can do analyses where date ranges overlap 10/1/21 20:11

flightsMatchDatesDash <- readRDS("~/Dropbox/Clients/PHI Social Media/flights.rds") %>% filter(str_detect(`Date of Service`, "-")) %>% 
  mutate(Date = as_date(`Date of Service`))

flightsDatesSlash <- readRDS("~/Dropbox/Clients/PHI Social Media/flights.rds") %>% filter(str_detect(`Date of Service`, "/")) %>% 
  mutate(Date = as.Date(mdy_hm(`Date of Service`)))

flights <- full_join(flightsMatchDatesDash, flightsDatesSlash) %>% 
  rename(`Exact Date` = "Date")

flightsMatchSM <- flights %>% filter(`Exact Date` > "2022-09-23") %>% 
  select(`Base Name`, `Exact Date`) %>% 
  mutate(Date = case_when(
    `Exact Date` > ymd('2022-09-23') & `Exact Date` <= ymd('2022-09-30') ~ ymd('2022-09-30'),
    `Exact Date` > ymd('2022-09-30') & `Exact Date` <= ymd('2022-10-09') ~ ymd('2022-10-09'),
    `Exact Date` > ymd('2022-10-09') & `Exact Date` <= ymd('2022-10-16') ~ ymd('2022-10-16'),
    `Exact Date` > ymd('2022-10-16') & `Exact Date` <= ymd('2022-10-23') ~ ymd('2022-10-23'),
    `Exact Date` > ymd('2022-10-23') & `Exact Date` <= ymd('2022-10-30') ~ ymd('2022-10-30'),
    `Exact Date` > ymd('2022-10-30') & `Exact Date` <= ymd('2022-11-06') ~ ymd('2022-11-06'),
    `Exact Date` > ymd('2022-11-06') & `Exact Date` <= ymd('2022-11-13') ~ ymd('2022-11-13'),
    `Exact Date` > ymd('2022-11-13') & `Exact Date` <= ymd('2022-11-20') ~ ymd('2022-11-20'),
    `Exact Date` > ymd('2022-11-20') & `Exact Date` <= ymd('2022-11-27') ~ ymd('2022-11-27'),
    `Exact Date` > ymd('2022-11-27') & `Exact Date` <= ymd('2022-12-04') ~ ymd('2022-12-04'),
    `Exact Date` > ymd('2022-12-04') & `Exact Date` <= ymd('2022-12-11') ~ ymd('2022-12-11'),
    `Exact Date` > ymd('2022-12-11') & `Exact Date` <= ymd('2022-12-18') ~ ymd('2022-12-18'),
    `Exact Date` > ymd('2022-12-18') & `Exact Date` <= ymd('2022-12-25') ~ ymd('2022-12-25'),
    `Exact Date` > ymd('2022-12-25') & `Exact Date` <= ymd('2023-01-01') ~ ymd('2023-01-01'),
    `Exact Date` > ymd('2023-01-01') & `Exact Date` <= ymd('2023-01-08') ~ ymd('2023-01-08'),
    `Exact Date` > ymd('2023-01-08') & `Exact Date` <= ymd('2023-01-15') ~ ymd('2023-01-15'),
    `Exact Date` > ymd('2023-01-15') & `Exact Date` <= ymd('2023-01-22') ~ ymd('2023-01-22'),
    `Exact Date` > ymd('2023-01-22') & `Exact Date` <= ymd('2023-02-01') ~ ymd('2023-02-01'),
    `Exact Date` > ymd('2023-02-01') & `Exact Date` <= ymd('2023-02-05') ~ ymd('2023-02-05'),
    `Exact Date` > ymd('2023-02-05') & `Exact Date` <= ymd('2023-02-12') ~ ymd('2023-02-12'),
    `Exact Date` > ymd('2023-02-12') & `Exact Date` <= ymd('2023-02-19') ~ ymd('2023-02-19'),
    `Exact Date` > ymd('2023-02-19') & `Exact Date` <= ymd('2023-02-26') ~ ymd('2023-02-26')
  )) 
 
flight_volume <-flightsMatchSM %>% 
  #mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>% 
  count %>% 
  mutate(week_length = case_when(
    Date == ymd('2022-09-30') ~ 7,
    Date == ymd('2022-10-09') ~ 9,
    Date == ymd('2022-10-16') ~ 7,
    Date == ymd('2022-10-23') ~ 7,
    Date == ymd('2022-10-30') ~ 7,
    Date == ymd('2022-11-06') ~ 7,
    Date == ymd('2022-11-13') ~ 7,
    Date == ymd('2022-11-20') ~ 7,
    Date == ymd('2022-11-27') ~ 7,
    Date == ymd('2022-12-04') ~ 7,
    Date == ymd('2022-12-11') ~ 7,
    Date == ymd('2022-12-18') ~ 7,
    Date == ymd('2022-12-25') ~ 7,
    Date == ymd('2023-01-01') ~ 7,
    Date == ymd('2023-01-08') ~ 7,
    Date == ymd('2023-01-15') ~ 7,
    Date == ymd('2023-01-22') ~ 7,
    Date == ymd('2023-02-01') ~ 10,
    Date == ymd('2023-02-05') ~ 4,
    Date == ymd('2023-02-12') ~ 7,
    Date == ymd('2023-02-19') ~ 7,
    Date == ymd('2023-02-26') ~ 7
  )) %>% 
  mutate(flightsPerDay = n/week_length) %>% 
  select(Date, flightsPerDay)
  
mutate(Date = format(as.POSIXct(`Date of Service`, format = '%m/%d/%Y %H:%M'), format = "%m-%d-%Y"))#, format = "%m-%d-%Y")) #....filter(Year > 2021)
socialMedia <- readRDS("~/Dropbox/Clients/PHI Social Media/socialMedia.rds")

# transform dates in flightsMatchDates to give Year, Month, Week columns.  will require lots of groupings to get proper week.  Ask them to do that.

sm_wide <- socialMedia %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(week_length = case_when(
    Date == ymd('2022-09-30') ~ 7,
    Date == ymd('2022-10-09') ~ 9,
    Date == ymd('2022-10-16') ~ 7,
    Date == ymd('2022-10-23') ~ 7,
    Date == ymd('2022-10-30') ~ 7,
    Date == ymd('2022-11-06') ~ 7,
    Date == ymd('2022-11-13') ~ 7,
    Date == ymd('2022-11-20') ~ 7,
    Date == ymd('2022-11-27') ~ 7,
    Date == ymd('2022-12-04') ~ 7,
    Date == ymd('2022-12-11') ~ 7,
    Date == ymd('2022-12-18') ~ 7,
    Date == ymd('2022-12-25') ~ 7,
    Date == ymd('2023-01-01') ~ 7,
    Date == ymd('2023-01-08') ~ 7,
    Date == ymd('2023-01-15') ~ 7,
    Date == ymd('2023-01-22') ~ 7,
    Date == ymd('2023-02-01') ~ 10,
    Date == ymd('2023-02-05') ~ 4,
    Date == ymd('2023-02-12') ~ 7,
    Date == ymd('2023-02-19') ~ 7,
    Date == ymd('2023-02-26') ~ 7
  )) %>% 
  mutate(platform_metric = paste0(Platform, " ", Metric)) %>% 
  mutate(scorePerDay = Score/week_length) %>% 
  group_by(Date, platform_metric) %>% 
  summarise(sum_score = sum(scorePerDay, na.rm=TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = platform_metric, values_from = sum_score)

flightsSM <- full_join(flight_volume, sm_wide) %>% ungroup()

flightsSMnorm <- flightsSM %>% 
  mutate_at(c("flightsPerDay",   "Facebook ER",     "Facebook Fans",   "Facebook Posts",  "Instagram ER",    "Instagram Fans",  "Instagram Posts",
               "Twitter ER",      "Twitter Fans",    "Twitter Posts"), funs(z = scale(.))) %>% 
  select(Date, flightsPerDay_z:`Twitter Posts_z`) %>% 
  pivot_longer(2:11) 

  #colnames(flightsSMnorm)[3] <- "z-score"

corrs <- flightsSM %>% select(-(Date)) %>%  correlate() %>% focus(flightsPerDay)


data <- flightsSMnorm %>% filter(name == "flightsPerDay_z" | name == "Facebook ER_z")

ggplot(data = data, aes(x = Date, y =value, group=name, color = name)) +
  geom_line()

# drive_trash("datesForPhi")
# gs4_create("datesForPhi", sheets = as_tibble(unique(socialMedia$Date)))
# drive_mv("datesForPhi", "~/DCI/CPI reports/PHI/social media analysis/")

  

