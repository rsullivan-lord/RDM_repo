#Rachel Sullivan-Lord RDM Course Data Cleaning Project
#September 23 2026

#install any of the following packages you don't yet have
#use: install.packages("name of package")
#then load each with library

library(assertr)
library(stringdist) 
library(tidyverse)
library(GGally)
library(here)
library(skimr)
library(dplyr)

#update renv lockfile if needed with renv::snapshot()

## set a plotting theme
theme_set(theme_bw())

#confirm project root path
here::i_am("scripts/RSLdataclean.R")
here("data", "chl-a-samples-messy.csv")
here("data", "stations-messy.csv")
here("data", "waterbodies-messy.csv")

#Read in the raw data
#Chlorophyll A samples raw
chl_a_samples_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "chl-a-samples-messy.csv"
  )
)
#Stations raw
stations_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "stations-messy.csv"
  )
)
#Waterbodies raw
waterbodies_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "waterbodies-messy.csv"
  )
)

#Data exploration Waterbodies messy
dim(waterbodies_messy_raw) #[1] 87 rows (observations) 2 columns (variables)
head(waterbodies_messy_raw, 5)
summary(waterbodies_messy_raw)
skimr::skim(waterbodies_messy_raw)
glimpse(waterbodies_messy_raw)

#Data exploration Stations messy
dim(stations_messy_raw) #[1] 97 rows (observations) 6 columns (variables)
head(stations_messy_raw, 5)
summary(stations_messy_raw)
skimr::skim(stations_messy_raw)
glimpse(stations_messy_raw)
#Visual inspection of stations_messy_raw shows likely coordinate typo for ST052

#Make sure latitude is positive
stations_messy_raw %>% 
   verify(latitude > 0)

# verification [latitude > 0] failed! (1 failure) 
# 
# verb redux_fn    predicate column index value
# 1 verify       NA latitude > 0     NA    52    NA      Indicates where problem is

#Make sure longitude is negative
stations_messy_raw %>% 
  verify(longitude < 0)  
# verification [longitude < 0] failed! (2 failures)
# 
# verb redux_fn     predicate column index value
# 1 verify       NA longitude < 0     NA     5    NA
# 2 verify       NA longitude < 0     NA    52    NA

#Add clean versions as new columns and include column indicating any alterations (=TRUE)
stations_clean <- stations_messy_raw %>%
  mutate(
    flagged = latitude < 0 & longitude > 0,
    latitude_clean = if_else(flagged, longitude, latitude),
    longitude_clean = if_else(flagged, latitude, longitude),
    sign_flipped = longitude_clean > 0,
    longitude_clean = if_else(sign_flipped, -longitude_clean, longitude_clean),
    altered = flagged | sign_flipped #include column indicating row has been altered
  )%>%
  select(-latitude, -longitude, -flagged, -sign_flipped) #drop extra columns

#check that the fix worked
stations_clean %>%
  verify(latitude_clean > 0) %>%
  verify(longitude_clean < 0)

#Save stations_clean in processed data folder
write_csv(stations_clean, "data/processed/stations_clean.csv")

#Data exploration Chlorophyll samples messy
dim(chl_a_samples_messy_raw) #[1] 1578 rows (observations) and 15 columns (variables)
head(chl_a_samples_messy_raw, 5)
summary(chl_a_samples_messy_raw)
skimr::skim(chl_a_samples_messy_raw)
glimpse(chl_a_samples_messy_raw)

#Create empty log to document cleaning changes
#Each row documents a cleaning decision, assumption, or unresolved issue
cleaning_log <- tibble::tibble(
  log_id = integer(),
  issue_or_decision = character(),
  processing_area = character(),
  affected_records = character(),
  action_taken = character(),
  status = character()
)

cleaning_log <- tibble::tribble(
  ~issue_or_decision, ~affected_records, ~action_taken, ~status,
  
  "Swapped coordinate values and missing sign",
  "ST005; ST052",
  "Fixed values, created new columns, created altered column indicating change",
  "Documented decision",
) |>
  mutate(log_id = row_number()) |>
  select(log_id, everything())

write_csv(cleaning_log, "outputs/cleaning_log.csv")




