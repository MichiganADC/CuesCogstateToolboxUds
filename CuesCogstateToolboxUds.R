#!/usr/bin/env Rscript

## Script to summarize some of the data in the "LaptopToolboxDataset" spreadsheet

library(dplyr)
library(lubridate)
library(ggplot2)

df <- readr::read_csv("LaptopToolboxDataset_DATA_2018-03-07.csv", na = "", trim_ws = TRUE)

# Check data integrity
length(unique(df$subject_id))
length(unique(df$mrn))
length(unique(df$madc_id))

which(duplicated(df$subject_id) == duplicated(df$mrn) %in% FALSE) + 1
which(duplicated(df$subject_id) == duplicated(df$madc_id) %in% FALSE) + 1
which(duplicated(df$mrn) == duplicated(df$madc_id) %in% FALSE) + 1

#####
### Check dates
##
grep(pattern = "dob", x = names(df))
grep(pattern = "date", x = names(df))
length(grep(pattern = "date", x = names(df)))

# Coerce df$dob to type Date
class(df$dob)
lubridate::mdy(df$dob)
df$dob <- lubridate::mdy(df$dob)

# Coerce any columns with "date" in field name to type Date
df %>% 
  select(matches("date")) # this doesn't work because there's a non-date 'remdates' column;
                          # use ends_with("dates") instead
missing_dates_1 <- df %>% 
  select(ends_with("date")) %>% 
  sapply(X = ., FUN = function(x) { sum(is.na(x)) })
missing_dates_2 <- df %>% 
  mutate_at(vars(ends_with("date")), mdy) %>% 
  select(ends_with("date")) %>% 
  sapply(X = ., FUN = function(x) { sum(is.na(x)) })
identical(missing_dates_1, missing_dates_2)
df <- df %>% 
  mutate_at(vars(ends_with("date")), mdy)
# Histo plot of columns with dates
ggplot(df, aes(x = dob)) + geom_histogram()
df_dates <- df %>% 
  select(subject_id, redcap_event_name, dob, ends_with("date"))
lapply(X = df_dates[, -(1:2)], FUN = function(x) { # is column
    ggplot(df, aes(x = x)) + geom_histogram(bins = 20) + ggtitle(label = names(x))
  })
# Range of columns with dates
lapply(X = df_dates[, -(1:2)], FUN = function(x) { range(x, na.rm = TRUE) })
df_dates_future <- df_dates %>% 
  filter_at(vars(ends_with("date")), any_vars(. > Sys.Date()))
lapply(X = df_dates_future[, -(1:2)], FUN = function(x) { sum(x > Sys.Date(), na.rm = TRUE) })
lapply(X = df_dates_future[, -(1:2)], FUN = function(x) { x > Sys.Date() })

#####
### Check columns with numeric values
##
# df_num
df_num <- df %>% 
  select_if(is.numeric) 
df_num_range <- lapply(X = df_num, FUN = function(x) { range(x, na.rm = TRUE) })

####
###
##
#
df_char <- df %>% 
  select_if(is.character)

# df_factor <- df %>% # no factor columns
#   select_if(is.factor)

# df_logic <- df %>% # no logical columns
#   select_if(is.logical)







