library(class)
library(modelr)
library(car)
library(xgboost)
library(randomForest)
library(glmnet)


set.seed(10)


training_data <- sample_frac(df_all_years, .75)
testing_data <- anti_join(df_all_years, training_data)


stop()
#### Logistic Regression #################


linear_model <- glm(lost_money ~ ., data = training_data, family = 'binomial')
summary(linear_model)
vif(linear_model)

testing_predictions <- add_predictions(testing_data, linear_model, var = 'linear_prob') %>% 
 mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
        ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
 

linear_accuracy <- tibble(mean(testing_predictions$linear_pred_count))


stop()
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

stop()
###### Lasso ############


cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = 'binomial')
best_lambda <- cv_lasso$lambda.min

x_val <- model.matrix(~ ., data = select(testing_data, -lost_money, -pred_ridge, -accuracy_ridge))
lasso_model <- glmnet(x_train, y_train, alpha = 1, family = 'binomial')

testing_data_glm <- testing_data %>% 
  mutate(pred_lasso = predict(lasso_model, s = best_lambda, newx = x_val, type = 'class')
         ,accuracy_lasso = ifelse(pred_lasso == lost_money, 1, 0))

lasso_accuracy <- tibble(mean(testing_data_glm$accuracy_lasso))


stop()
###### KNN ############


# training_predictors <- model.matrix(~ ., data = select(training_data, -lost_money))
# testing_predictors <- model.matrix(~ ., data = select(testing_data, -lost_money))
# training_response <- select(training_data, lost_money) %>%  as.matrix()
# 
# knn_model <- knn(training_predictors, testing_predictors, training_response, k = 3)
# 
# testing_knn <- testing_data %>% 
#   mutate(accuracy = ifelse(lost_money == knn_model, 1, 0))
# 
# knn_accuracy <- tibble(mean(testing_knn$accuracy))


########## Graph of Grid Search for Best K #################

knn_accuracies_quest <- read_csv('data_lessVariables/knn_accuracies.csv') %>% 
  filter(!accuracy == 0) %>% 
  arrange(desc(accuracy))

knn_accuracy <- knn_accuracies_quest$accuracy

ggplot(knn_accuracies_quest, aes(x = k, y = accuracy)) +
  geom_point(size = 4) +
  ggtitle('Grid Search for Best k') +
  labs(x = 'K', y = 'Accuracy')


#### Boosted Tree ##########

# x <- select(training_data, -lost_money) 
# y <- training_data$lost_money %>% as.matrix()
# 
# xgboost_cv <- xgb.cv(data = model.matrix(~ ., data = x)
#                         , label = y
#                         , nrounds = 10000
#                         , early_stopping_rounds = 10
#                         , nfold = 10
#                         , objective = "binary:logistic"
#                         ,verbose = FALSE)
# 
# xgboost_model <- xgboost(data = model.matrix(~ ., data = x)
#                         , label = y
#                         , nrounds = 100
#                         , eta = .1
#                         , gamma = 5
#                         , max_depth = 20
#                         , objective = "binary:logistic"
#                         , verbose = FALSE)
# 
# dtrain <- xgb.DMatrix(data = model.matrix(~ ., data = testing_data))
# 
# testing_data_boost <- testing_data %>% 
#   mutate(prob_boost = predict(xgboost_model, dtrain)
#          ,pred_boost = ifelse(prob_boost >= .5, 1, 0)
#          ,accuracy_boost = ifelse(pred_boost == lost_money, 1, 0))
# 
# boosted_accuracy <- tibble(mean(testing_data_boost$accuracy_boost))


######### Graph for Best XgBoost Parameters ############


xgboost_accuracies_quest <- read_csv('data_lessVariables/boost_accuracies.csv') %>% 
  filter(!error == 0) %>% 
  mutate(accuracy = round(1 - error, 3)) %>% 
  select(-error) %>% 
  arrange(desc(accuracy)) %>% 
  head(10) 

boosted_accuracy <- xgboost_accuracies_quest$accuracy

kable(xgboost_accuracies_quest)


########## Random Forest ##################


# rf_model <- randomForest(lost_money ~ ., data = training_data, ntrees = 500)
# 
# testing_data_rf <- testing_data %>% 
#   mutate(pred_rf = predict(rf_model, testing_data))
# 
# rf_accuracy <- tibble(mean(testing_data_rf$accuracy))


######### Graph for Best Random Forest Parameters ############


rf_accuracies_quest <- read_csv('data_lessVariables/rf_grid_search1.csv') %>% 
  filter(!accuracy == 0) %>% 
  arrange(accuracy) %>% 
  head(10) 

rf_accuracy <- rf_accuracies_quest$accuracy

kable(rf_accuracies_quest)


########## Combined Accuracies #####################

accuracies <- tibble(model = c('Logistic Regression', 'Ridge Regression', 'Lasso', 'KNN', 'Boosted Tree')
                     ,accuracies <- c(linear_accuracy, ridge_reg_accuracy, lasso_accuracy, knn_accuracy, bosted_accuracy))

accuracies <- as_tibble(bind_rows(accuracies_names, accuracies_list))

kable(accuracies, align = c(rep('c', 5)))

# write_csv(combined_accuracies, 'data/model_accuracies')
