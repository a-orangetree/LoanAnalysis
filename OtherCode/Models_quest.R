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
         ,-late_fees_flag, -has_next_payment_flag, -next_payment_due_amount, -next_payment_due_date
         ,-total_fees_paid, -principal_paid, -principal_balance, -total_paid, -interest_paid
         ,-service_fees_paid, -prosper_fees_paid, -age_in_months)

df_all_years <- drop_na(df_all_years)

set.seed(10)

training_data <- sample_frac(df_all_years, .75)
testing_data <- anti_join(df_all_years, training_data)

#### Logistic Regression #################

linear_model <- glm(lost_money ~ ., data = training_data, family = 'binomial')
summary(linear_model)
vif(linear_model)

testing_predictions <- add_predictions(testing_data, linear_model, var = 'linear_prob') %>% 
  mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
         ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))


linear_accuracy <- tibble(mean(testing_predictions$linear_pred_count))


###### Ridge Regression ############


x_train <- model.matrix(~ ., data = select(training_data, -lost_money))
y_train <- training_data$lost_money %>% as.matrix()

cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, family = 'binomial')
best_lambda <- cv_ridge$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data, -lost_money))
ridge_model <- glmnet(x_train, y_train, alpha = 0, family = 'binomial')

testing_data_glm <- testing_data %>% 
  mutate(pred_ridge = predict(ridge_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_ridge = ifelse(pred_ridge == lost_money, 1, 0))

ridge_reg_accuracy <- tibble(mean(testing_data_glm$accuracy_ridge))


###### Lasso ############


cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = 'binomial')
best_lambda <- cv_lasso$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data, -lost_money, -pred_ridge, -accuracy_ridge))
lasso_model <- glmnet(x_train, y_train, alpha = 1, family = 'binomial')

testing_data_glm <- testing_data %>% 
  mutate(pred_lasso = predict(lasso_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_lasso = ifelse(pred_lasso == lost_money, 1, 0))

lasso_accuracy <- tibble(mean(testing_data_glm$accuracy_lasso))


###### KNN ############


training_predictors <- model.matrix(~ ., data = select(training_data, -lost_money))
testing_predictors <- model.matrix(~ ., data = select(testing_data, -lost_money))
training_response <- select(training_data, lost_money) %>%  as.matrix()

knn_accuracies <- tibble(k = 0, accuracy = 0)

knn_function <- function(list_of_k) {

  for (x in list_of_k) {

    knn_model <- knn(training_predictors, testing_predictors, training_response, k = x)

    testing_knn <- testing_data %>%
      mutate(accuracy = ifelse(lost_money == knn_model, 1, 0))

    knn_accuracies <- add_row(knn_accuracies, k = x, accuracy = mean(testing_knn$accuracy))

    write_csv(knn_accuracies, '/projects/p30546/LoanAnalysis/knn_accuracies.csv')
  }
}

list_of_k <- list(1, 3, 5, 10, 25, 50, 75, 100)
knn_function(list_of_k)

#### Boosted Tree ##########


boost_accuracies <- tibble(number_of_rounds = 0, best_iteration = 0, best_ntreelimit = 0
                          , eta = 0, gamma = 0, max_depth = 0, error = 0)

x <- select(training_data, -lost_money) 
y <- training_data$lost_money %>% as.matrix()

boosted_func <- function(number_of_rounds, learning_rates, list_of_gammas, max_depths) {

  for (round in number_of_rounds) {
    for (rate in learning_rates) {
      for (gamma in list_of_gammas) {
        for (depth in max_depths) {

            xgboost_cv <- xgb.cv(data = model.matrix(~ ., data = x)
                                    , label = y
                                    , early_stopping_rounds = 10
                                    , nfold = 10
                                    , objective = "binary:logistic"
                                    ,verbose = FALSE
                                    , nrounds = round
                                    , eta = rate
                                    , gamma = gamma
                                    , max_depth = depth)

            boost_accuracies <- add_row(boost_accuracies
                                        , number_of_rounds = round
                                        , best_iteration = xgboost_cv$best_iteration
                                        , best_ntreelimit = xgboost_cv$best_ntreelimit
                                        , eta = rate
                                        , gamma = gamma
                                        , max_depth = depth
                                        , error = xgboost_cv$evaluation_log$test_error_mean[xgboost_cv$best_iteration])

            write_csv(boost_accuracies, 'data/boost_accuracies.csv')
        }
      }
    }
  }
}

number_of_rounds <- list(100, 500, 1000, 2500, 5000, 10000)
learning_rates <- list(.001, .003, .005, .01, .03, .05, .1)
list_of_gammas <- list(1, 5, 20, 50, 100)
max_depths <- list(2, 4, 6, 10, 20)

boosted_func(number_of_rounds, learning_rates, list_of_gammas, max_depths)


########## Random Forest ##################


rf_grid_search <- tibble(number_of_trees = 0, mtry = 0, min_observations = 0, terminal_nodes = 0, accuracy = 0)

rf_function <- function(number_of_trees, mtries, min_observations, terminal_nodes) {
  
  for (tree_size in number_of_trees) {
    for (n_pred in mtries) {
      for (min_obs in min_observations) {
        for (term_nodes in terminal_nodes) {
  
            rf_model <- randomForest(lost_money ~ ., data = df_all_years
                           , ntrees = tree_size
                           , mtry = n_pred
                           , nodesize = min_obs
                           , maxnodes = term_nodes)
  
            df_all_years_rf <- df_all_years %>% 
              mutate(pred_rf = predict(rf_model)
                    ,accuracy_class = ifelse(pred_rf == lost_money, 1, 0))
 
            rf_accuracy <- mean(df_all_years_rf$accuracy_class)
            print(rf_accuracy)
            
            rf_grid_search <- add_row(rf_grid_search, number_of_trees = tree_size
                                      ,mtry = n_pred
                                      ,terminal_nodes = term_nodes
                                      ,min_observations = min_obs
                                      ,accuracy = rf_accuracy)
        }
        write_csv(rf_grid_search, 'data/rf_grid_search.csv')
      }
    }
  }
}

num_of_predictors_bag <- length(names(df_all_years))

number_of_trees <- seq(500, 10000, 500)
mtries <- seq(1, num_of_predictors_bag - 1)
min_observations <- list(1, 10, 25, 50, 100, 150)
terminal_nodes <- list(100, 500, 1000, 2500)

rf_function(number_of_trees, mtries, min_observations, terminal_nodes)