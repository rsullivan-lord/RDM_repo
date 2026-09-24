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

#Read in raw chlorophyll A data
chl_a_samples_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "chl-a-samples-messy.csv"
  )
)

#Read in Stations raw
stations_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "stations-messy.csv"
  )
)

#Read in Waterbodies raw
waterbodies_messy_raw <- readr::read_csv(
  here::here(
    "data",
    "raw",
    "waterbodies-messy.csv"
  )
)

#Data exploration waterbodies-messy.csv
dim(waterbodies_messy_raw) #[1] 87 rows (observations) 2 columns (variables)
head(waterbodies_messy_raw, 5)
summary(waterbodies_messy_raw)
skimr::skim(waterbodies_messy_raw)

#── Data Summary ────────────────────────
# Values               
# Name                       waterbodies_messy_raw
# Number of rows             87                   
# Number of columns          2                    
# _______________________                         
# Column type frequency:                          
#   character                2                    
# ________________________                        
# Group variables            None                 
# 
# ── Variable type: character ─────────────────────────────────────────────────────────────
# skim_variable  n_missing complete_rate min max empty n_unique whitespace
# 1 waterbody_code         0             1   5   5     0       87          0
# 2 waterbody_name         0             1   4  23     0       87          0
# glimpse(waterbodies_messy_raw)

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

#Capitalize all waterbody names and create new clean dataframe with column indicating what was alterated
waterbodies_clean <- waterbodies_messy_raw %>%
  mutate(
    waterbody_name_clean = str_replace_all(waterbody_name, "quinn lake", "Quinn Lake"),
    name_fixed = waterbody_name !=waterbody_name_clean,
    altered = name_fixed
    ) %>%
  select(-name_fixed, -waterbody_name)

#Save cleaned waterbodies csv file
write_csv(waterbodies_clean, "data/processed/waterbodies_clean.csv")

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

#Fix variable classes for sample_volume_filtered_ml and absorbance_663nm from character to numeric
chl_a_samples_clean <- chl_a_samples_messy_raw %>%
  mutate(
    across(c(sample_volume_filtered_ml, absorbance_663nm), as.numeric)
  )
#returns error so identify values that are causing the problem
chl_a_samples_messy_raw %>%
  filter(
    (!is.na(sample_volume_filtered_ml) & is.na(suppressWarnings(as.numeric(sample_volume_filtered_ml)))) |
      (!is.na(absorbance_663nm) & is.na(suppressWarnings(as.numeric(absorbance_663nm))))
  )
## A tibble: 2 × 15
# chl_a_sample_code station_code date       sample_replicate subsample_replicate
# <chr>             <chr>        <chr>                 <dbl>               <dbl>
#   1 CHL0154           ST095        1994-09-14                1                  NA
# 2 CHL0163           ST069        1998-05-25                1                  NA

#Visual inspection of rows identified shows likely typos involving commas where period should be, where no comma should be, unnecessary unit "mL"

#Fix issues with the absorbance_663nm and sample_volume_filtered_ml variables
chl_a_samples_clean <- chl_a_samples_messy_raw %>%
  mutate(
    altered = coalesce( #identify which rows were altered =TRUE
      (row_number() == 154 & str_detect(absorbance_663nm, ",")) |
      (row_number() == 163 & str_detect(sample_volume_filtered_ml,
                                        regex(",|mL", ignore_case = TRUE))),
      FALSE
    ), 

    absorbance_663nm = if_else(
      row_number() == 154, #specifically selects one problem
      str_replace(absorbance_663nm, ",", "."), #replaces comma with period
      absorbance_663nm
    ),
    
    sample_volume_filtered_ml = if_else(
      row_number() == 163, #specifically selects one problem
      sample_volume_filtered_ml %>%
        str_remove_all(",") %>% #change 1,000 to 1000
        str_remove_all(regex("mL", ignore_case=TRUE)) %>% #removes unnecessary unit label
        str_trim(), #gets rid of any white space created by deletion
      sample_volume_filtered_ml
    ),
    
    across(c(absorbance_663nm, sample_volume_filtered_ml), as.numeric) #changes class from character to numeric
  )

#change date variable from character to date
chl_a_samples_clean2 <- chl_a_samples_clean %>%
  mutate(date = ymd(date))
# Warning message:
#   There was 1 warning in `mutate()`.
# ℹ In argument: `date = ymd(date)`.
# Caused by warning:
#   !  3 failed to parse. 

#What rows were turned to NA when they could not be parsed
which(is.na(chl_a_samples_clean2$date))
#[1]  507  524 1008

#any misformatted dates?
chl_a_samples_clean %>%
  filter(is.na(ymd(date, quiet = TRUE)))%>%
  select(date)
# A tibble: 3 × 1
# date         
# <chr>        
#   1 June 14, 1999
# 2 28/06/1999   
# 3 10/15/2002  

#Fix individual lines
chl_a_samples_clean3 <- chl_a_samples_clean %>%
  mutate(
    date_txt = str_squish(str_replace_all(date, "\u00a0", " ")),
    parsed = ymd(date_txt, quiet = TRUE),
    changed = is.na(parsed) & !is.na(date_txt), 
    parsed = coalesce(parsed,
                      as.Date(date_txt, format = "%B %d, %Y"),
                      as.Date(date_txt, format = "%d/%m/%Y"),
                      as.Date(date_txt, format = "%m/%d/%Y")),
    altered = if_else(changed, TRUE, altered),
    date = parsed
    ) %>%
  select(-date_txt, -parsed, -changed)
#Check that it worked, this = 0
sum(is.na(chl_a_samples_clean3$date))
#[1] 0



cleaning_log <- tibble::tribble(
  ~issue_or_decision, ~affected_records, ~action_taken, ~status,
  
  "Swapped station coordinate values and missing sign",
  "ST005; ST052",
  "Fixed values, created new columns, created altered column indicating change",
  "Documented decision",
  
  "Waterbody capitalization issues",
  "WB075",
  "replaced all stringr to affect quinn lake string",
  "Documented decision",
  
  "Changed comma to period, removed comma and removed unit label",
  "CHL0154; CHL0163",
  "Fixed values, changed class to numeric, identified in altered column",
  "Documented decision",
  
  "Chl samples date format corrections",
  "CHL0507; CHL0524; CHL1008",
  "Individual fixes to YMD, identified in altered column",
  "Documented decision"
) |>
  mutate(log_id = row_number()) |>
  select(log_id, everything())

#save log in outputs
write_csv(cleaning_log, "outputs/cleaning_log.csv")




