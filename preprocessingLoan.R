library(tidyverse)
library(lubridate)
library(gridExtra)
library(knitr)
library(ggthemes)

theme_set(theme_fivethirtyeight())

######################
# Import Loan data


# Have problems
# df_2005 <- read_csv('data/Loans_20050101to20130101_20180415T060005.csv') # (753 rows)
# df_2016 <- read_csv('data/Loans_20160101to20170101_20180415T060742.csv') %>% mutate(year = 2016) 


df_2013 <- read_csv('data/Loans_20130101to20140101_20180415T060158.csv') %>% mutate(year = 2013)
df_2014 <- read_csv('data/Loans_20140101to20150101_20180415T060226.csv') %>% mutate(year = 2014)
df_2015 <- read_csv('data/Loans_20150101to20160101_20180415T060359.csv') %>% mutate(year = 2015)
df_2017 <- read_csv('data/Loans_20170101to20180101_20180415T060911.csv') %>% mutate(year = 2017)
df_2018 <- read_csv('data/Loans_20180101toCurrent_20180415T061114.csv') %>% mutate(year = 2018)


df_2018 <- df_2018 %>% mutate(loan_default_reason = as.integer(loan_default_reason))


df_all_years <- bind_rows(df_2013, df_2014, df_2015, df_2017, df_2018)
df_all_years_raw <- df_all_years

unique(df_all_years$loan_status_description)


# FOLLOW-UP: did we define money lost correctly? Needs confirmation.
# This is tied to the FOLLOW-UP below.
df_all_years <- df_all_years %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED' & next_payment_due_amount == 0, 0, 1)
                             , levels = c(0, 1))
         ,late_fees_flag = factor(ifelse(late_fees_paid == 0, 0, 1), levels = c(0, 1))
         ,has_next_payment_flag = factor(ifelse(next_payment_due_amount == 0, 0, 1), levels = c(0, 1))
         ,prosper_rating = factor(prosper_rating, levels = c('AA', 'A', 'B', 'C', 'D', 'E', 'HR'))
         ,term = factor(term, levels = c(12, 36, 60))
         ,loan_status_description = factor(loan_status_description
                                           , levels = c("CHARGEOFF","COMPLETED","DEFAULTED","CURRENT","CANCELLED"))
         ,service_fees_paid = service_fees_paid * -1
         ,total_paid = principal_paid + service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         ,total_fees_paid = service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         # ,month_of_origination = factor(month(origination_date))
         )

######################
# Clean Loan data


# Remove loans which are currently outstanding or were cancelled
# df_all_years <- df_all_years %>% 
#   filter(loan_status_description == 'COMPLETED' & next_payment_due_amount == 0
#          ,loan_status_description %in% c('DEFAULTED', 'CHARGEOFF'))


# Determines the number of NAs in each column
# sapply(df_all_years, function(x) sum(is.na(x)))


# Removes redundant or non-useful columns
# Removed loan_default_reason[_description] because of the high number of NAs (600K+)
# Removed loan_status[_description] because it contains too much information for training
df_all_years <- df_all_years %>%
  select(-loan_default_reason, -loan_default_reason_description, -loan_status, -loan_status_description)


# Remove duplicates - Commented out since it doesn't remove anything
# print("Duplicated rows removed: ")
# sum(duplicated(df_all_years))
# df_all_years <- unique(df_all_years)


# Remove rows with NA 
# df_all_years <- drop_na(df_all_years) # Do not do this. Will remove too much data.