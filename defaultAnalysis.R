library(tidyverse)
library(modelr)
library(car)
library(lubridate)
library(class)
library(gbm)
library(gridExtra)
library(knitr)

# TODO:
# 1. Figure out error for linear model
# 2. Add cross-validation
# 3. Implement KNN
# 4. Figure out performance issue with Boosting 
# 5. PCA?

######################
# Import LOAN data

# df_2005 <- read_csv('data/Loans_20050101to20130101_20180415T060005.csv') # Has problems (753 rows)
df_2013 <- read_csv('data/Loans_20130101to20140101_20180415T060158.csv') %>% mutate(year = 2013)
df_2014 <- read_csv('data/Loans_20140101to20150101_20180415T060226.csv') %>% mutate(year = 2014)
df_2015 <- read_csv('data/Loans_20150101to20160101_20180415T060359.csv') %>% mutate(year = 2015)
# df_2016 <- read_csv('data/Loans_20160101to20170101_20180415T060742.csv') %>% mutate(year = 2016)
df_2017 <- read_csv('data/Loans_20170101to20180101_20180415T060911.csv') %>% mutate(year = 2017)
df_2018 <- read_csv('data/Loans_20180101toCurrent_20180415T061114.csv') %>% 
  mutate(year = 2018
         ,loan_default_reason = as.integer(loan_default_reason))

df_all_years <- bind_rows(df_2013, df_2014, df_2015, df_2017, df_2018)
df_all_years_raw <- df_all_years

# Change datatypes of select columns to factor
df_all_years <- df_all_years %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED', 1, 0)) #need to confirm this!!!!!!!!!!!!!!!!!!!!
         ,late_fees_flag = factor(ifelse(late_fees_paid == 0, 0, 1))
         ,prosper_rating = factor(prosper_rating)
         ,service_fees_paid = service_fees_paid * -1
         ,total_paid = principal_paid + service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         ,total_fees_paid = service_fees_paid + interest_paid + prosper_fees_paid + late_fees_paid
         # ,month_of_origination = factor(month(origination_date)) creates error when modeling???????
         ,term = factor(term))

glimpse(df_all_years)

#####################
# Descriptive Statistics


aggregates <- df_all_years %>%
  group_by(lost_money, prosper_rating) %>%
  summarise(count = n()
            ,median_amt_borrowed = median(amount_borrowed)
            ,total_paid = median(total_paid)
            ,total_fees_paid = median(total_fees_paid)
            ,median_principal_paid = median(principal_paid)
            ,median_service_fees = median(service_fees_paid))

aggregates %>% arrange(prosper_rating, lost_money) %>% kable()


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


######################
# Clean LOAN data based on EDA above


# Remove loans which are currently outstanding or were cancelled
df_all_years <- df_all_years %>% 
  filter(loan_status_description == 'COMPLETED' & next_payment_due_amount == 0
         , loan_status_description %in% c('DEFAULTED', 'CHARGEOFF'))

# Remove redundant or non-useful columns
df_all_years <- df_all_years %>%
  select(-loan_default_reason, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description, -loan_status_description, -next_payment_due_date,
         -origination_date, -interest_paid, -borrower_rate, -principal_balance)

# Remove duplicates
print("Duplicated rows removed: ")
sum(duplicated(df_all_years))
df_all_years <- unique(df_all_years)

# Remove rows with NA - Do not do. This will remove too much data.
# before_na <- dim(df_all_years)[1]
# df_all_years <- drop_na(df_all_years)
# after_na <- dim(df_all_years)[1]
# print("Rows with NA removed: ")
# after_na - before_na

training_data <- sample_frac(df_all_years, .8)
testing_data <- anti_join(df_all_years, training_data)


#############################
# Best Subset Selection

num_predictors <- dim(df_all_years)[2] - 1

best_subset_model <- regsubsets(lost_money ~ ., data = df_all_years, nvmax = dim(df_all_years)[2] - 1)
best_subset_summary <- summary(best_subset_model)

names(best_subset_summary)
# Create tibble which contains data from results object
best_subset_results <- tibble(num_pred = 1:num_predictors
                              ,rss = best_subset_summary$rss
                              ,rsquared = best_subset_summary$rsq
                              ,adj_rsquared = best_subset_summary$adjr2
                              ,cp = best_subset_summary$cp
                              ,bic = best_subset_summary$bic)

# RSS
plot1 <- best_subset_results %>% 
  ggplot(aes(num_pred, rss)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(best_subset_results$rss)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('RSS')

# ADJ R-SQUARED
plot2 <- best_subset_results %>% 
  ggplot(aes(num_pred, adj_rsquared)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.max(best_subset_results$adj_rsquared)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Adj R-squared')

# CP
plot3 <- best_subset_results %>% 
  ggplot(aes(num_pred, cp)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(best_subset_results$cp)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('Cp')

# BIC
plot4 <- best_subset_results %>% 
  ggplot(aes(num_pred, bic)) + 
  geom_point() +
  geom_vline(aes(xintercept = which.min(best_subset_results$bic)), color = 'red') +
  xlab('Number of Predictors') +
  ylab('BIC')

# Each of the measures comes up with vastly different number of 
# predictors... 
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

# Selecting a model. Display coefficients
coef(best_subset_model, 10)


#############################
# Linear Model

linear_model <- glm(lost_money ~ ., data = training_data, family = 'binomial')
summary(linear_model)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Check multicollinearity
# vif(linear_model)
# 
# 
# training_predictions <- add_predictions(df_2014, linear_model, var = 'linear_prob') %>% 
#   mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
#          ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
# 
# mean(training_predictions$linear_pred_count)
# 
# 
# testing_predictions <- add_predictions(df_2015, linear_model, var = 'linear_prob') %>% 
#   mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
#          ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
# 
# mean(testing_predictions$linear_pred_count)


############################
# LDA

lda_model <- MASS::lda(lost_money ~ ., data = training_data)

training_predictions <- training_data %>% 
  mutate(lda_pred = predict(lda_model, training_data)$class
        ,lda_pred_count = ifelse(lda_pred == lost_money, TRUE, FALSE))

mean(training_predictions$lda_pred_count)


testing_predictions <- testing_data %>% 
  mutate(lda_pred = predict(lda_model, testing_data)$class
         ,lda_pred_count = ifelse(lda_pred == lost_money, TRUE, FALSE))

mean(testing_predictions$lda_pred_count)


stop()
##########################
# KNN


training_predictors <- select(training_data, -lost_money)
testing_predictors <- select(testing_data, -lost_money)
training_response <- training_data$lost_money

knn_model <- knn(training_predictors, testing_predictors, training_response, k = 3) # Error?????????????


##########################
# Boosted Tree


boosted_model <- gbm(lost_money ~ ., data = training_data, distribution = 'bernoulli'
                     , n.trees = 500, interaction.depth = 2, shrinkage = .01)
summary(boosted_model)

boosted_predictions <- predict.gbm(boosted_model, testing_data, n.trees = 500, type = 'response') #Wtf????????????????
