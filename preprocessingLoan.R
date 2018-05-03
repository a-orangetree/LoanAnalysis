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


# FOLLOW-UP: did we define money lost correctly? Needs confirmation.
# This is tied to the FOLLOW-UP below.
df_all_years <- df_all_years %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED' & next_payment_due_amount == 0, 0, 1)
                             , levels = c(0, 1))
         ,late_fees_flag = factor(ifelse(late_fees_paid == 0, 0, 1))
         ,has_next_payment_flag = factor(ifelse(next_payment_due_amount == 0, 0, 1), levels = c(0, 1))
         ,prosper_rating = factor(prosper_rating, levels = c('AA', 'A', 'B', 'C', 'D', 'E', 'HR'))
         ,term = factor(term)
         ,year = factor(year)
         ,loan_status_description = factor(loan_status_description)
         ,service_fees_paid = service_fees_paid * -1
         ,total_paid = principal_paid + service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         ,total_fees_paid = service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         ,month_of_origination = factor(month(origination_date)))


#####################
# EDA


aggregates <- df_all_years %>%
  group_by(lost_money, prosper_rating) %>%
  summarise(count = n()
            ,median_amt_borrowed = median(amount_borrowed)
            ,total_paid = round(median(total_paid))
            ,total_fees_paid = round(median(total_fees_paid))
            ,median_principal_paid = round(median(principal_paid))
            ,median_service_fees = round(median(service_fees_paid)))

aggregates %>% arrange(prosper_rating, lost_money) %>% kable(align=c(rep('c', ncol(.))))


## FOLLOW-UP: why do we notice the below (those row counts should be zero)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Notice we have some "COMPLETED" with payments due....
unique(filter(df_all_years_raw, next_payment_due_amount != 0)$loan_status_description)
have_payment_due <- filter(df_all_years_raw, next_payment_due_amount != 0)
glimpse(filter(have_payment_due, loan_status_description == 'COMPLETED'))
arrange(count(have_payment_due, loan_status_description), desc(n))


# And we have some "CURRENT" with no payments due...
unique(filter(df_all_years_raw, next_payment_due_amount == 0)$loan_status_description)
have_NO_payment_due <- filter(df_all_years_raw, next_payment_due_amount == 0)
glimpse(filter(have_NO_payment_due, loan_status_description == 'CURRENT'))
arrange(count(have_NO_payment_due, loan_status_description), desc(n))


# Counts by YEAR
df_all_years %>% 
  ggplot() +
  geom_bar(aes(year, fill = lost_money))

df_all_years %>% 
  group_by(year, lost_money) %>% 
  summarise(count = n()) %>%
  spread(key = year, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Counts by MONTH
df_all_years %>% 
  ggplot() +
  geom_bar(aes(month_of_origination, fill = lost_money))

df_all_years %>% 
  group_by(month_of_origination, lost_money) %>% 
  summarise(count = n()) %>% 
  spread(key = month_of_origination, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Counts by TERM
df_all_years %>% 
  ggplot() +
  geom_bar(aes(term, fill = lost_money))

df_all_years %>% 
  group_by(term, lost_money) %>% 
  summarise(count = n()) %>% 
  spread(key = term, value = count) %>% 
  kable(align=c(rep('c', ncol(.))))


# Plots AMOUNT BORROWED by rating
# Clearly the more you lend, the more likely you'll lose money
ggplot(df_all_years) +
  geom_histogram(aes(amount_borrowed, binwidth = 20000), fill = "black") +
  facet_grid(lost_money ~ prosper_rating) +
  scale_x_continuous(labels = c())

# ggplot(df_all_years, aes(lost_money, amount_borrowed)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, amount_borrowed)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)


# Plots BORROWER RATE by rating
# Unfortunately, the higher the rate, the higher the chances you'll lose money 
ggplot(df_all_years) +
  geom_histogram(aes(borrower_rate), fill = "black") +
  facet_grid(lost_money ~ prosper_rating)

# ggplot(df_all_years, aes(lost_money, borrower_rate)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, borrower_rate)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)


# Plots TOTAL FEES PAID by rating
# Loans which lose money also have higher fees (perhaps driven by higher loan amounts?)...
ggplot(df_all_years) +
  geom_histogram(aes(total_fees_paid), fill = "black") +
  facet_grid(lost_money ~ prosper_rating) +
  scale_x_continuous(labels = c())

# ggplot(df_all_years, aes(lost_money, total_fees_paid)) +
#   geom_boxplot() +
#   facet_wrap(~ prosper_rating)

ggplot(df_all_years, aes(prosper_rating, total_fees_paid)) +
  geom_boxplot() +
  facet_wrap(~ lost_money)

stop()
######################
# Clean Loan data


# Remove loans which are currently outstanding or were cancelled
df_all_years <- df_all_years %>% 
  filter(loan_status_description == 'COMPLETED' & next_payment_due_amount == 0
         ,loan_status_description %in% c('DEFAULTED', 'CHARGEOFF'))


# Remove redundant or non-useful columns
df_all_years <- df_all_years %>%
  select(-loan_default_reason, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description, -loan_status_description, -next_payment_due_date,
         -origination_date, -interest_paid, -borrower_rate, -principal_balance, -year)


# Remove duplicates
print("Duplicated rows removed: ")
sum(duplicated(df_all_years))
df_all_years <- unique(df_all_years)


# Remove rows with NA 
# df_all_years <- drop_na(df_all_years) # Do not do this. Will remove too much data.