library(tidyverse)
library(lubridate)

# TODO:

# 1. What is Channel Code?
# 2. How do we know if a borrower has defaulted just by using this data?

########################
# Import PEOPLE data


people_2005 <- read_csv('~/Desktop/people_data/people_2005to2013.csv') %>% 
  mutate(file = 2005
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

people_2013 <- read_csv('~/Desktop/people_data/people_2013.csv') %>% 
  mutate(file = 2013
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

people_2014 <- read_csv('~/Desktop/people_data/people_2014.csv') %>% 
  mutate(file = 2014
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

people_2015 <- read_csv('~/Desktop/people_data/people_2015.csv') %>% 
  mutate(file = 2015
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

people_2016 <- read_csv('~/Desktop/people_data/people_2016.csv') %>% 
  mutate(file = 2016
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

people_2017 <- read_csv('~/Desktop/people_data/people_2017.csv') %>% 
  mutate(file = 2017
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd)
         ,prosper_score = as.integer(prosper_score))

people_2018 <- read_csv('~/Desktop/people_data/people_2018.csv') %>% 
  mutate(file = 2018
         ,prior_prosper_loans31dpd = as.integer(prior_prosper_loans31dpd)
         ,prior_prosper_loans61dpd = as.integer(prior_prosper_loans61dpd))

# df_all_people <- bind_rows(people_2005, people_2013, people_2014, people_2015, people_2016, people_2017,
#                           people_2018) %>% 
df_all_people <- bind_rows(people_2017, people_2018)%>% 
  mutate(credit_pull_year = year(credit_pull_date)
         ,credit_pull_month = month(credit_pull_date)
         ,listing_start_year = year(listing_start_date)
         ,listing_start_month = month(listing_start_date)
         ,listing_end_year = year(listing_end_date)
         ,listing_end_month = month(listing_end_date)
         ,listing_creation_year = year(listing_creation_date)
         ,listing_creation_month = month(listing_creation_date)
         ,loan_origination_year = year(loan_origination_date)
         ,loan_origination_month = month(loan_origination_date)
         ,listing_status = factor(listing_status)
         ,listing_status_reason = factor(listing_status_reason)
         ,group_indicator = factor(group_indicator)
         ,lender_indicator = factor(lender_indicator)
         ,income_range_description = factor(income_range_description)
         ,income_verifiable = factor(income_verifiable)
         ,listing_term = factor(listing_term)
         ,listing_category_id = factor(listing_category_id)
         ,prosper_rating = factor(prosper_rating)
         ,partial_funding_indicator = factor(partial_funding_indicator)
         ,employment_status_description = factor(employment_status_description)
         ,prosper_score = factor(prosper_score)
         ,funding_threshold = factor(funding_threshold))


########################
# Clean PEOPLE data

# count(df_all_people, employment_status_description)
# unique(df_all_people$occupation)
# count(df_all_people, income_verifiable)
# count(df_all_people, borrower_metropolitan_area)
# count(df_all_people, verification_stage)
# count(df_all_people, listing_status_reason)
# count(df_all_people, group_indicator)
# count(df_all_people, group_name)
# count(df_all_people, employment_status_description)
# count(df_all_people, prosper_score)
# count(df_all_people, scorex_change)
# count(df_all_people, funding_threshold)
# count(df_all_people, prosper_rating)
# count(df_all_people, fico_score)

df_all_people <- unique(df_all_people) %>% 
  filter(income_verifiable == 'True', !listing_status_reason %in% c('Cancelled', 'Withdrawn')) %>% 
  select(-file, -income_verifiable, -borrower_metropolitan_area, -verification_stage, -listing_status
         ,-group_name, -income_range, -scorex_change, -credit_pull_date, -listing_start_date
         ,-listing_end_date, -listing_creation_date, -loan_origination_date, -amount_funded, -amount_remaining
         ,-channel_code, -prosper_score, -estimated_return, -estimated_loss_rate)

glimpse(df_all_people)

#####################
# EDA

# MEMBER ID identifies borrowers, whereas listing number identifies a request to borrow
count(df_all_people, member_key) %>% 
  arrange(desc(n))
# glimpse(filter(df_all_people, member_key == 'BE863375303959526539C52'))

# BY STATE
count(df_all_people, borrower_state) %>% 
  arrange(desc(n))

# RATE BY INCOME - Not meaningful??
ggplot(filter(df_all_people, income_range_description != 'Not displayed')) +
  geom_point(aes(x = income_range_description, y = borrower_rate))

# RATE BY RATING
ggplot(df_all_people) +
  geom_point(aes(x = fico_score, y = borrower_rate))

# RATE BY FICO - Meaningless
ggplot(df_all_people) +
  geom_point(aes(x = fico_score, y = prosper_rating))

# RATE BY LISTING ID
ggplot(df_all_people) +
  geom_point(aes(x = listing_category_id, y = borrower_rate))

# RATE BY 
ggplot(filter(df_all_people, !is.na(prior_prosper_loans))) +
  geom_point(aes(x = prior_prosper_loans_late_cycles, y = borrower_rate))



stop()
##############################
# Clustering

# Not running because of NAs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# kmeans_model <- kmeans(df_all_people, 2)
# kmeans_model
