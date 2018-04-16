library(tidyverse)
library(knitr)
library(car)
library(modelr)
library(randomForest)


######################
# Import data

df_2014 <- read_csv('~/Downloads/Loans_20140101to20150101_20180415T060226.csv')
df_2013 <- read_csv('~/Downloads/Loans_20130101to20140101_20180415T060158.csv')


######################
# Clean data


df_2014 <- df_2014 %>% 
  filter(loan_status_description != 'CURRENT') %>% 
  filter(prosper_rating != 'N/A') %>% 
  mutate(lost_money = ifelse(loan_status_description == 'COMPLETED', 1, 0)
         ,prosper_rating = factor(prosper_rating))


# Did Not Lose Money = TRUE
# Lose Money = False
df_2013 <- df_2013 %>% 
  filter(loan_status_description != 'CURRENT') %>% 
  mutate(lost_money = ifelse(loan_status_description == 'COMPLETED', 1, 0)
         ,prosper_rating = factor(prosper_rating)) 


df_2013 <- df_2013 %>% 
  select(-loan_default_reason, -interest_paid, -principal_paid, -service_fees_paid, -prosper_fees_paid, -late_fees_paid,
         -origination_date, -age_in_months, -loan_number, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description)


# Drop NAs
dim(df_2013)
df_2013 <- drop_na(df_2013)
dim(df_2013)


#####################
# Descriptive Statistics


aggregates <- df_2013 %>% 
  group_by(lost_money, prosper_rating) %>% 
  transmute(median_amt_borrowed = median(amount_borrowed)
            ,mean_amt_borrowed = mean(amount_borrowed)
            ,median_rate = median(borrower_rate)
            ,mean_rate = mean(borrower_rate)
            ,average_term = mean(term)
            ,count = n()) 


(aggregates <- unique(aggregates) %>% 
    arrange(prosper_rating, lost_money))


df_2013 %>% 
  group_by(lost_money) %>% 
  ggplot() +
  geom_bar(aes(borrower_rate, fill = lost_money))


#############################
# Linear Model

# Note: Add cross-validation


linear_model <- lm(lost_money ~ ., data = df_2013)
summary(linear_model)


# Check multicollinearity
vif(linear_model)


df_2014 <- add_predictions(df_2014, linear_model, var = 'linear_pred') %>% 
  mutate(linear_pred = ifelse(linear_pred > .5, 1, 0)
         ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))

glimpse(df_2014)


# Percent Accuracy
mean(df_2014$linear_pred_count)

##########################
# Decision Tree

stop()


tree_model <- randomForest(lost_money ~ ., data = df_2013, mtry = length(names(df_2013)) - 1, ntree = 5000)
summary(tree_model)
