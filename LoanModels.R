library(class)
library(modelr)
library(car)
library(xgboost)
library(randomForest)
library(glmnet)


#### Logistic Regression #################

df_all_years_linear <- df_all_years %>%
  select(-interest_paid, -borrower_rate, -principal_balance, 
         -principal_paid, -total_paid, -total_fees_paid)


training_data_linear <- sample_frac(df_all_years_linear, .8)
testing_data_linear <- anti_join(df_all_years, training_data_linear)


linear_model <- glm(lost_money ~ ., data = training_data_linear, family = 'binomial')
summary(linear_model)
vif(linear_model)


testing_predictions <- add_predictions(testing_data_linear, linear_model, var = 'linear_prob') %>% 
 mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
        ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
 

mean(testing_predictions$linear_pred_count) ~.64


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

mean(testing_data_glm$accuracy_ridge) ~.75


###### Lasso ############


lasso_model <- glmnet(x_train, y_train, alpha = 1, family = 'binomial')
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = 'binomial')

best_lambda <- cv_lasso$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data_glm, -lost_money, -pred_ridge, -accuracy_ridge))

testing_data_glm <- testing_data_glm %>% 
  mutate(pred_lasso = predict(lasso_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_lasso = ifelse(pred_lasso == lost_money, 1, 0))

mean(testing_data_glm$accuracy_lasso)


###### KNN ############

# training_predictors <- select(training_data, -lost_money)
# testing_predictors <- select(testing_data, -lost_money)
# training_response <- training_data$lost_money
# 
# knn_model <- knn(model.matrix(~., training_predictors), model.matrix(~., testing_predictors), training_response, k = 3)


#### Boosted Tree ##########

training_data_boost <- sample_frac(drop_na(df_all_years), .8)
testing_data_boost <- anti_join(drop_na(df_all_years), training_data_boost)

x <- select(training_data_boost, -lost_money) 
y <- training_data_boost$lost_money %>% as.matrix()

# xgboost_cv <- xgb.cv(data = model.matrix(~ ., data = x)
#                         , label = y
#                         , nrounds = 10000
#                         , early_stopping_rounds = 10
#                         , nfold = 10
#                         , objective = "binary:logistic"
#                         ,verbose = FALSE)

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

mean(testing_data_boost$accuracy_boost)


########## Random Forest ##################

training_data_rf <- sample_frac(df_all_years, .8)
testing_data_rf <- anti_join(df_all_years, training_data_rf)

rf_model <- randomForest(lost_money ~ ., data = training_data_rf, ntrees = 500)

testing_data_rf <- testing_data_rf %>% 
  mutate(pred_rf = predict(rf_model, testing_data))