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
months_separate <- lapply(months_separate, function(x) select_if(x, is.numeric))
# Count missing values
count_na <- lapply(month_separate, function(x) sum(is.na(x)))
# Pick the month with the lowest number of missing values - it's march
march <- months_separate[[which.min(unlist(count_na))]]
# How many missing values in each column?
march_na <- march %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  gather(variable, count_na) %>%
  arrange(desc(count_na)) %>%
  mutate(percentage_na = count_na/nrow(march))
# Remove columns, for which less than 50% data is present
march_less <- select_if(march, function(x) sum(is.na(x)) < 200)
# Save ready datasets
save(march, file = "march.rda")
save(march_less, file = "march_less.rda")
