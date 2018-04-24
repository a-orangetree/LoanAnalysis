library(tidyverse)
library(modelr)
library(car)
library(lubridate)
library(leaps)
library(class)
library(gbm)
library(gridExtra)
library(caret)

# TODO:
# 1. Figure out error for linear model
# 2. Add cross-validation
# 3. Implement KNN
# 4. Figure out performance issue with Boosting 
# 5. PCA?

######################
# Import data

# df_2005 <- read_csv('data/Loans_20050101to20130101_20180415T060005.csv') # Has problems (753 rows)

df_2013 <- read_csv('data/Loans_20130101to20140101_20180415T060158.csv') %>% 
  mutate(year = '2013')
df_2014 <- read_csv('data/Loans_20140101to20150101_20180415T060226.csv') %>% 
  mutate(year = '2014')
df_2015 <- read_csv('data/Loans_20150101to20160101_20180415T060359.csv') %>% 
  mutate(year = '2015')
# df_2016 <- read_csv('data/Loans_20160101to20170101_20180415T060742.csv') %>%
# mutate(year = '2016')
df_2017 <- read_csv('data/Loans_20170101to20180101_20180415T060911.csv') %>% 
  mutate(year = '2017')
df_2018 <- read_csv('data/Loans_20180101toCurrent_20180415T061114.csv') %>% 
  mutate(year = '2018'
         ,loan_default_reason = as.integer(loan_default_reason))

df_all_years <- bind_rows(df_2013, df_2014, df_2015, df_2017, df_2018)
df_all_year_raw <- df_all_years

######################
# Clean data

# Remove loans which are currently outstanding or were cancelled
df_all_years <- df_all_years %>% 
  filter(!loan_status_description %in% c('CURRENT', 'CANCELLED'))

# Remove loans with no rating
df_all_years <- df_all_years %>% 
  filter(prosper_rating != 'N/A')

# Change datatypes of select columns to factor
df_all_years <- df_all_years %>% 
  mutate(lost_money = factor(ifelse(loan_status_description == 'COMPLETED', 1, 0))
         ,prosper_rating = factor(prosper_rating)
         # ,month_of_origination = factor(month(origination_date)) ????????????????????????
         ,term = factor(term))

# Remove redundant or non-useful columns
df_all_years <- df_all_years %>%
  select(-loan_default_reason, -loan_number, -days_past_due, -debt_sale_proceeds_received, -loan_status,
         -loan_default_reason_description, -loan_status_description, -next_payment_due_date, -principal_paid,
         -origination_date, -interest_paid, -borrower_rate)

# Remove duplicates
print("Duplicated rows removed: ")
sum(duplicated(df_all_years))
df_all_years <- unique(df_all_years)

# Remove rows with NA
before_na <- dim(df_all_years)[1]
df_all_years <- drop_na(df_all_years)
after_na <- dim(df_all_years)[1]
print("Rows with NA removed: ")
after_na - before_na

training_data <- sample_frac(df_all_years, .7)
testing_data <- anti_join(df_all_years, training_data)


#####################
# Descriptive Statistics

# aggregates <- df_all_years %>%
#   group_by(lost_money, prosper_rating) %>%
#   summarise(median_amt_borrowed = median(amount_borrowed)
#             ,median_rate = median(borrower_rate)
#             ,count = n())
# 
# (aggregates <- unique(aggregates) %>%
#     arrange(prosper_rating, lost_money))


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
training_predictors <- select(training_data, -lost_money)
testing_response <- select(testing_data, lost_money)

trControl <- trainControl(method  = "cv", number  = 5)

fit <- train(lost_money ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = training_data)

# knn_model <- knn(training_predictions, testing_predictors, training_response, k = )


##########################
# Boosted Tree

# df_2014[rowSums(is.na(df_2014)) > 0,]
# 
# boosted_model <- gbm(lost_money ~ ., data = df_2014, distribution = 'bernoulli'
#                      , n.trees = 5000, interaction.depth = 2, shrinkage = .01)
# summary(boosted_model)
