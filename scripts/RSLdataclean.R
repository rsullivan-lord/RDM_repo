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
glimpse(waterbodies_messy_raw)

#Create empty log to document cleaning changes
#Each row documents a cleaning decision, assumption, or unresolved issue
cleaning_log <- tibble::tibble(
  log_id = integer(),
  issue_or_decision = character(),
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
      chl_a_sample_code == "CHL0154", #specifically selects the problem row ID
      str_replace(absorbance_663nm, ",", "."), #replaces comma with period
      absorbance_663nm
    ),
    
    sample_volume_filtered_ml = if_else(
      chl_a_sample_code == "CHL0163", #specifically selects one problem
      sample_volume_filtered_ml %>%
        str_remove_all(",") %>% #change 1,000 to 1000
        str_remove_all(regex("mL", ignore_case=TRUE)) %>% #removes unnecessary unit label
        str_trim(), #gets rid of any white space created by deletion
      sample_volume_filtered_ml
    ),
    
    across(c(absorbance_663nm, sample_volume_filtered_ml), as.numeric) #changes class from character to numeric
  )

#change date variable from character to date
date_check <- chl_a_samples_clean %>%
  mutate(date = ymd(date))
# Warning message:
#   There was 1 warning in `mutate()`.
# ℹ In argument: `date = ymd(date)`.
# Caused by warning:
#   !  3 failed to parse. 

#What rows were turned to NA when they could not be parsed
which(is.na(date_check$date))
#[1]  507  524 1008

#Check if there are any dates not in YMD format
chl_a_samples_clean %>%
  filter(is.na(ymd(date, quiet = TRUE)))%>%
  select(date)
# A tibble: 3 × 1
# date         
# <chr>        
#   1 June 14, 1999
# 2 28/06/1999   
# 3 10/15/2002  

#Fix these 3 individual lines to YMD format
chl_a_samples_clean2 <- chl_a_samples_clean %>%
  mutate(
    date_txt = str_squish(str_replace_all(date, "\u00a0", " ")), #in case there are any hidden text problems
    parsed = ymd(date_txt, quiet = TRUE),
    changed = is.na(parsed) & !is.na(date_txt), 
    parsed = coalesce(parsed,
                      as.Date(date_txt, format = "%B %d, %Y"),
                      as.Date(date_txt, format = "%d/%m/%Y"),
                      as.Date(date_txt, format = "%m/%d/%Y")),
    altered = if_else(changed, TRUE, altered), #marks as altered, keeps previous other altered values
    date = parsed
    ) %>%
  select(-date_txt, -parsed, -changed)

#Check that it worked, there are 0 NA values in $date
sum(is.na(chl_a_samples_clean2$date))
#[1] 0

#Ensure all measurements are positive
chl_a_samples_clean2%>%
  assert(within_bounds(0, Inf),
         -c(chl_a_sample_code, station_code, date, sample_replicate, subsample_replicate, description, unit, altered))
#Column 'extract_volume_ml' violates assertion 'within_bounds(0, Inf)' 1 time
# verb redux_fn             predicate            column index value
# 1 assert       NA within_bounds(0, Inf) extract_volume_ml   141  -9.8
# 
# Error: assertr stopped execution

#Remove negative sign from any values in column, even though only one identified
chl_a_samples_clean2 <- chl_a_samples_clean2 %>%
  mutate(
    had_dash = str_detect(extract_volume_ml, "-"), #identify any negative values
    extract_volume_ml = str_remove_all(extract_volume_ml, "-"), #remove all negatives
    altered = if_else(coalesce(had_dash, FALSE), TRUE, altered) #mark altered if had negative sign
  ) %>%
  select(-had_dash) #remove column once values marked as altered

#Ensure it worked, should return 0
sum(str_detect(chl_a_samples_clean2$extract_volume_ml, "-"), na.rm = TRUE)

#Check if all sample_replicate values between 1-5
chl_a_samples_clean2%>%
  verify(sample_replicate >= 1 & sample_replicate <= 5)
#verification [sample_replicate >= 1 & sample_replicate <= 5] failed! (12 failures)
#These NA could be a problem, exclude from dataset, or not

#All sample_volume_filtered_ml should be <= 1000
chl_a_samples_clean2%>%
  verify(sample_volume_filtered_ml <= 1000)

#verification [sample_volume_filtered_ml <= 1000] failed! (1 failure)
# 
# verb redux_fn                         predicate column index value
# 1 verify       NA sample_volume_filtered_ml <= 1000     NA     6    NA

#Fix likely typo from 10000 to 1000, mark as altered
chl_a_samples_clean2 <- chl_a_samples_clean2 %>%
  mutate(
    fix_volume = chl_a_sample_code == "CHL0006" & sample_volume_filtered_ml == 10000, #selects problem by sample code
    sample_volume_filtered_ml = if_else(fix_volume, 1000, sample_volume_filtered_ml),
    altered = if_else(fix_volume, TRUE, altered)
  )%>%
  select(-fix_volume)

#Check fix worked, all values are <= 1000
chl_a_samples_clean2 %>%
  verify(sample_volume_filtered_ml <= 1000)

#Check extract is <= 1000, max sample volume filtered
chl_a_samples_clean2 %>%
  mutate(extract_volume_ml = as.numeric(extract_volume_ml))

#Check class of extract_volume_ml, should be numeric
class(chl_a_samples_clean2$extract_volume_ml) #it shows its character

#Make sure no values will interfer with converting class to numeric
chl_a_samples_clean2%>%
  filter(is.na(as.numeric(extract_volume_ml)) & !is.na(extract_volume_ml))%>%
  select(extract_volume_ml)

#Change class to numeric
chl_a_samples_clean2 <-chl_a_samples_clean2%>%
  mutate(extract_volume_ml = as.numeric(extract_volume_ml))

#Verify extracted values are less than sample volume
chl_a_samples_clean2%>%
  verify(extract_volume_ml <= 1000)

#confirm class
class(chl_a_samples_clean3$extract_volume_ml)

#Make sure no NAs within 2 Chl 16 and 20 variables
which(is.na(chl_a_samples_clean3$chl_a_16ed) | is.na(chl_a_samples_clean3$chl_a_20ed))

#Check for outliers, each row's mahalanobis distance is within 4 median absolute deviations of all the distances
find_outliers <- function(col, k = 4) {
  med_val <- median(col, na.rm = TRUE)
  mad_val <- mad(col, na.rm = TRUE)
  col < (med_val - k * mad_val) | col > (med_val + k * mad_val)
}

#Add outliers as own column in dataset
chl_a_samples_clean3 <- chl_a_samples_clean3 %>%
  mutate(
    outliers = find_outliers(chl_a_16ed) | find_outliers(chl_a_20ed)
  )

#check it worked
sum(chl_a_samples_clean3$outliers) #Sum = 57

#Save chl_a_samples_clean3 in processed data folder
write_csv(chl_a_samples_clean3, "data/processed/chl_a_samples_clean3.csv")

#Combine waterbodies and stations by waterbody_code
head(waterbodies_clean)
head(stations_clean)

#Join dataframes with combined altered column indicating changes
stations_waterbodies <- stations_clean %>%
  left_join(waterbodies_clean, by = "waterbody_code", suffix = c("_station", "_waterbody"))%>%
  mutate(altered = altered_station | altered_waterbody) %>%
  select(-altered_station, -altered_waterbody)

#Check and make sure this is 0 so everything has a match
sum(is.na(stations_waterbodies$waterbody_name_clean))

#Join dataframes with combined altered column indicating changes
combined_data <- chl_a_samples_clean3 %>%
  left_join(
    stations_waterbodies %>% rename(altered_station = altered),
    by = "station_code"
  ) %>%
  mutate(altered = coalesce(altered, FALSE) | coalesce(altered_station, FALSE)) %>%
  select(-altered_station)

#check matching worked
sum(is.na(combined_data$waterbody_code))

combined_data %>%
  filter(is.na(waterbody_code))%>%
  select(chl_a_sample_code, station_code)
# A tibble: 1 × 2
# chl_a_sample_code station_code
# <chr>             <chr>       
#   1 CHL0271           ST999 #ST999 does not exist, judging by date, replicate number and subsample replicate number it should be ST097

#Change from ST999 to ST097 in chl_a_samples_clean3
chl_a_samples_clean3 <- chl_a_samples_clean3%>%
  mutate(
    altered = if_else(station_code == "ST999", TRUE, altered),
    station_code = if_else(station_code == "ST999", "ST097", station_code)
  )
sum(chl_a_samples_clean3$station_code == "ST999", na.rm = TRUE)

#Try join again station_waterbodies and chl_a_samples_clean3
combined_data <- chl_a_samples_clean3 %>%
  left_join(
    stations_waterbodies %>% rename(altered_station = altered),
    by = "station_code"
  ) %>%
  mutate(altered = coalesce(altered, FALSE) | coalesce(altered_station, FALSE)) %>%
  select(-altered_station)

#Check that it worked properly = 0
sum(is.na(combined_data$waterbody_code))

#Save stations_clean in processed data folder
write_csv(combined_data, "data/processed/combined_data_clean.csv")




#Record of changes made to datasets
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
  "Documented decision",
  
  "Chl samples negative value in extract_volume_ml",
  "CHL0141",
  "Removed all negative signs in column, identified in altered column",
  "Documented decision",
  
  "Typo in sample_volume_filtered_ml",
  "CHL0006",
  "Individual row fix from 10000 to 1000, identified in altered column",
  "Documented decision",
  
  "Wrong class for extract_volume_ml",
  "Whole variable extract_volume_ml",
  "Verified no values interfer with change, change from character to numeric",
  "Documented decision",
  
  "Nonexistant Station code ST999 preventing last dataset join to chl samples",
  "CHL0271; ST999",
  "Nearby values suggest typo, should be ST097, changed",
  "Documented decision"
) %>%
  mutate(log_id = row_number()) %>%
  select(log_id, everything())

#save log in outputs
write_csv(cleaning_log, "outputs/cleaning_log.csv")




