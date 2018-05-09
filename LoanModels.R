library(class)
library(gbm)
library(modelr)
library(car)
library(leaps)

# TODO:
# 1. Figure out error for linear model
# 2. Add cross-validation
# 3. Implement KNN
# 4. Figure out performance issue with Boosting 
# 5. PCA?


training_data <- sample_frac(df_all_years, .8)
testing_data <- anti_join(df_all_years, training_data)


#############################
# Linear Model


linear_model <- glm(lost_money ~ ., data = training_data, family = 'binomial')
summary(linear_model)

vif(linear_model)

testing_predictions <- add_predictions(testing_data, linear_model, var = 'linear_prob') %>% 
 mutate(linear_pred = ifelse(linear_prob > .5, 1, 0)
        ,linear_pred_count = ifelse(linear_pred == lost_money, TRUE, FALSE))
 
mean(testing_predictions$linear_pred_count)


stop()
##########################
# KNN

training_predictors <- select(training_data, -lost_money)
testing_predictors <- select(testing_data, -lost_money)
training_response <- training_data$lost_money

# knn_model <- knn(training_predictors, testing_predictors, training_response, k = 3) # Error?????????????
knn_model <- knn(model.matrix(~., training_predictors), model.matrix(~., testing_predictors), training_response, k = 3)


##########################
# Boosted Tree


boosted_model <- gbm(lost_money ~ ., data = training_data, distribution = 'bernoulli'
                     , n.trees = 500, interaction.depth = 2, shrinkage = .01)
summary(boosted_model)

boosted_predictions <- predict.gbm(boosted_model, testing_data, n.trees = 500, type = 'response') #Wtf????????????????
