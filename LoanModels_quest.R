library(class)
library(modelr)
library(xgboost)
library(randomForest)
library(glmnet)
library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(forcats)
library(tidyr)


### Import #############


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


df_all_years <- df_all_years %>%
  select(-loan_default_reason, -loan_default_reason_description, -loan_status, -loan_status_description
         ,-loan_number, -year, -origination_date, -days_past_due, -late_fees_paid, -debt_sale_proceeds_received
         ,-late_fees_flag, -has_next_payment_flag, -next_payment_due_amount, -next_payment_due_date)


#### Logistic Regression #################

df_all_years_linear <- df_all_years %>%
  select(-interest_paid, -borrower_rate, -principal_balance, 
         -principal_paid, -total_paid, -total_fees_paid)


training_data_linear <- sample_frac(df_all_years_linear, .8)
testing_data_linear <- anti_join(df_all_years, training_data_linear)


linear_model <- glm(lost_money ~ ., data = training_data_linear, family = 'binomial')
# summary(linear_model)
# vif(linear_model)


testing_predictions <- add_predictions(testing_data_linear, linear_model, var = 'linear_prob') %>% 
 mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
        ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
 

(linear_accuracy <- tibble(mean(testing_predictions$linear_pred_count))) #~.64


###### Ridge Regression ############

df_all_years_glmnet <- drop_na(df_all_years)

training_data_glm <- sample_frac(df_all_years_glmnet, .8)
testing_data_glm <- anti_join(df_all_years_glmnet, training_data_glm)

x_train <- model.matrix(~ ., data = select(training_data_glm, -lost_money))
y_train <- training_data_glm$lost_money %>% as.matrix()

ridge_model <- glmnet(x_train, y_train, alpha = 0, family = 'binomial')
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, family = 'binomial')

best_lambda <- cv_ridge$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data_glm, -lost_money))

testing_data_glm <- testing_data_glm %>% 
  mutate(pred_ridge = predict(ridge_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_ridge = ifelse(pred_ridge == lost_money, 1, 0))

(ridge_reg_accuracy <- tibble(mean(testing_data_glm$accuracy_ridge))) #~.75


###### Lasso ############


lasso_model <- glmnet(x_train, y_train, alpha = 1, family = 'binomial')
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = 'binomial')

best_lambda <- cv_lasso$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data_glm, -lost_money, -pred_ridge, -accuracy_ridge))

testing_data_glm <- testing_data_glm %>% 
  mutate(pred_lasso = predict(lasso_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_lasso = ifelse(pred_lasso == lost_money, 1, 0))

(lasso_accuracy <- tibble(mean(testing_data_glm$accuracy_lasso))) #~.88


###### KNN ############


data_for_knn <- drop_na(df_all_years)

training_data <- sample_frac(data_for_knn, .8)
testing_data <- anti_join(data_for_knn, training_data)

training_predictors <- model.matrix(~ ., data = select(training_data, -lost_money))
testing_predictors <- model.matrix(~ ., data = select(testing_data, -lost_money))
training_response <- select(training_data, lost_money) %>%  as.matrix()

knn_model <- knn(training_predictors, testing_predictors, training_response, k = 3)


testing_knn <- testing_data %>% 
  mutate(accuracy = ifelse(lost_money == knn_model, 1, 0))

(knn_accuracy <- tibble(mean(testing_knn$accuracy)))


#### Boosted Tree ##########

training_data_boost <- sample_frac(drop_na(df_all_years), .8)
testing_data_boost <- anti_join(drop_na(df_all_years), training_data_boost)

x <- select(training_data_boost, -lost_money) 
y <- training_data_boost$lost_money %>% as.matrix()

xgboost_cv <- xgb.cv(data = model.matrix(~ ., data = x)
                        , label = y
                        , nrounds = 10000
                        , early_stopping_rounds = 10
                        , nfold = 10
                        , objective = "binary:logistic"
                        ,verbose = FALSE)

xgboost_model <- xgboost(data = model.matrix(~ ., data = x)
                        , label = y
                        , nrounds = dim(xgboost_cv$evaluation_log)[1]
                        , objective = "binary:logistic"
                        , verbose = FALSE)

dtrain <- xgb.DMatrix(data = model.matrix(~ ., data = testing_data_boost))

testing_data_boost <- testing_data_boost %>% 
  mutate(prob_boost = predict(xgboost_model, dtrain)
         ,pred_boost = ifelse(prob_boost >= .5, 1, 0)
         ,accuracy_boost = ifelse(pred_boost == lost_money, 1, 0))

(boosted_accuracy <- tibble(mean(testing_data_boost$accuracy_boost))) #~.84


########## Random Forest ##################

df_all_year_rf <- drop_na(df_all_years)

training_data_rf <- sample_frac(df_all_years_rf, .8)
testing_data_rf <- anti_join(df_all_years_rf, training_data_rf)

rf_grid_search <- tibble(number_of_trees = 0, mtry = 0, min_observations = 0, terminal_nodes = 0, accuracy = 0)

rf_function <- function(number_of_trees, mtries, min_observations, terminal_nodes) {
  
  for (tree_size in number_of_trees) {
    for (n_pred in mtries) {
      for (min_obs in min_observations) {
        for (term_nodes in terminal_nodes) {
  
            rf_model <- randomForest(lost_money ~ ., data = training_data_rf
                           , ntrees = number_of_trees
                           , mtry = n_pred
                           , nodesize = min_obs
                           , maxnodes = term_nodes)
  
            testing_data_rf <- testing_data_rf %>% mutate(pred_rf = predict(rf_model, testing_data))
  
            rf_accuracy <- tibble(mean(testing_data_rf$accuracy))
            
            rf_grid_search <- add_row(rf_grid_search, number_of_trees = tree_size
                                      ,mtry = n_pred
                                      ,terminal_nodes = term_nodes
                                      ,min_observations = min_obs
                                      ,accuracy = rf_accuracy)
        }
        write_csv(rf_grid_search, '/projects/p30546/LoanAnalysis/rf_grid_search.csv')
      }
    }
  }
}

num_of_predictors_bag <- length(names(has_funding_rounds_rf))

number_of_trees <- seq(500, 10000, 500)
mtries <- seq(1, num_of_predictors_bag - 1)
min_observations <- list(1, 10, 25, 50, 100, 150)
terminal_nodes <- list(100, 500, 1000, 2500)

rf_function(number_of_trees, mtries, min_observations, terminal_nodes)

best_model <- read_csv('/projects/p30546/LoanAnalysis/rf_grid_search.csv') %>% 
  filter(accuracy != 0) %>% 
  arrange(desc(accuracy)) %>% 
  head(1)

rf_model <- randomForest(lost_money ~ ., data = training_data_rf
                         , ntrees = best_modelnumber_of_trees
                         , mtry = best_model$mtry
                         , nodesize = best_model$min_observations
                         , maxnodes = best_model$terminal_nodes)

testing_data_rf <- testing_data_rf %>% mutate(pred_rf = predict(rf_model, testing_data))

(rf_accuracy <- tibble(mean(testing_data_rf$accuracy)))


########## Combined Accuracies #####################

combined_accuracies <- bind_rows(linear_accuracy, ridge_reg_accuracy, lasso_accuracy, boosted_accuracy
                                 ,knn_accuracy, rf_accuracy)

write_csv(combined_accuracies, 'data/model_accuracies')
