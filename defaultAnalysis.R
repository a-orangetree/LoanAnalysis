library(tidyverse)


######################
# Import data

# df_2005 <- read_csv('data/Loans_20050101to20130101_20180415T060005.csv') # Has problems (753 rows)
df_2014 <- read_csv('data/Loans_20140101to20150101_20180415T060226.csv')
df_2013 <- read_csv('data/Loans_20130101to20140101_20180415T060158.csv')


######################
# Clean data


# Drop NAs
dim(df_2013)
df_2013 <- drop_na(df_2013)
dim(df_2013)

dim(df_2014)
df_2013 <- drop_na(df_2014)
dim(df_2014)


df_2014 <- df_2014 %>% 
  filter(loan_status_description != 'CURRENT') %>% 
  filter(prosper_rating != 'N/A') %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED', 1, 0))
         ,prosper_rating = factor(prosper_rating)
         ,term = factor(term))


# Did Not Lose Money = TRUE
# Lose Money = False
df_2013 <- df_2013 %>% 
  filter(loan_status_description != 'CURRENT') %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED', 1, 0))
         ,prosper_rating = factor(prosper_rating)
         ,term = factor(term)) 


df_2013 <- df_2013 %>% 
  select(-loan_default_reason, -interest_paid, -principal_paid, -service_fees_paid, -prosper_fees_paid, -late_fees_paid,
         -origination_date, -age_in_months, -loan_number, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description, -loan_status_description)


df_2014 <- df_2014 %>% 
  select(-loan_default_reason, -interest_paid, -principal_paid, -service_fees_paid, -prosper_fees_paid, -late_fees_paid,
         -origination_date, -age_in_months, -loan_number, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description, -loan_status_description)


#####################
# Descriptive Statistics


aggregates <- df_2013 %>%
  group_by(lost_money, prosper_rating) %>%
  summarise(median_amt_borrowed = median(amount_borrowed)
            ,mean_amt_borrowed = mean(amount_borrowed)
            ,median_rate = median(borrower_rate)
            ,mean_rate = mean(borrower_rate)
            ,average_term = mean(term)
            ,count = n())


(aggregates <- unique(aggregates) %>%
    arrange(prosper_rating, lost_money))


#############################
# Linear Model

# Note: Add cross-validation

linear_model <- glm(lost_money ~ ., data = df_2013, family = 'binomial')
summary(linear_model)
plot(linear_model, which = 1)

lda_model <- MASS::lda(lost_money ~ ., data = df_2013)

stop()
# Check multicollinearity
vif(linear_model) # Borrower Rate is the only VIF above 3 (it's 4)


# Training Error
df_2013 <- add_predictions(df_2013, linear_model, var = 'linear_prob') %>% 
  mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
         ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))

df_2013 <- add_predictions(df_2013, linear_model, var = 'lda_prob') %>% 
  mutate(lda_pred = ifelse(lda_prob > .5, 1, 0)
         ,lda_pred_count = ifelse(lda_pred == lost_money, TRUE, FALSE))


# Percent Accuracy
mean(df_2013$linear_pred_count)
mean(df_2013$lda_pred_count)


# Testing Error
df_2014 <- add_predictions(df_2014, linear_model, var = 'linear_prob') %>% 
  mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
         ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))

# Version 2
df_2014 <- df_2014 %>%
  mutate(linear_prob2 = predict(linear_model, df_2014, type = 'response')
         ,linear_pred2 = ifelse(linear_prob > .5, 1, 0)
         ,linear_pred_count2 = ifelse(linear_pred2 == lost_money, TRUE, FALSE))


df_2014 <- drop_na(df_2014)


# Percent Accuracy
mean(df_2014$linear_pred_count)

stop()
##########################
# Decision Tree


tree_model <- randomForest(lost_money ~ ., data = df_2013, mtry = length(names(df_2013)) - 1, ntree = 5000)
summary(tree_model)
