# Libraries ----
library(varclust)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
# Using data downloaded from http://bit.ly/2C8oqmp ----
# Unpack the data to the working directory.
# Import files
csv_files <- grep(".csv", x = list.files(), value = T) %>%
  grep("2017", x = ., value = T)
months_separate <- lapply(csv_files, function(x) read.csv(x))
# Cleaning & sanity check - missing data ----
# Remove non-numeric variables
months_separate2 <- lapply(months_separate, function(x) select_if(x, is.numeric))
# Count missing values
count_na <- lapply(months_separate2, function(x) sum(is.na(x)))
# Pick the month with the lowest number of missing values - it's march
march <- months_separate2[[which.min(unlist(count_na))]]
# How many missing values in each column?
march_na <- march %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  gather(variable, count_na) %>%
  arrange(desc(count_na)) %>%
  mutate(percentage_na = count_na/nrow(march))
# Remove columns, for which less than 50% data is present
march_less <- select_if(march, function(x) sum(is.na(x)) < 200)
march_less <- mlcc.preprocess(march_less)
march_daily <- months_separate[[3]] %>%
  mutate(UTC.time = as.character(UTC.time)) %>%
  mutate(UTC.time = as_date(UTC.time)) %>%
  group_by(UTC.time) %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  select_if(function(x) sum(is.na(x)) <= 0.5*length(x))
# Save ready datasets
save(march, file = "march.rda")
save(march_less, file = "march_less.rda")
save(march_daily, file = "march_daily.rda")
# Proposition for the future:
mlcc.preprocess <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = T), x))
  # %>% # opcjonalnie
    # as.matrix()
}
# Stations in Krakow
sensor_locations <- read_csv("sensor_locations.csv")
stations_meas <- colnames(march_less)
stations_meas <- str_replace(stations_meas, "X", "")
stations <- str_split(stations_meas, "_", simplify = T)[, 1]
meas <- str_split(stations_meas, "_", simplify = T)[, 2]
stations <- tibble(id = as.integer(stations),
                   meas = meas) %>%
  left_join(sensor_locations, by = "id")

save(stations, file = "stations.rda")
