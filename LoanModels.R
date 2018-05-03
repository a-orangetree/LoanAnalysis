library(class)
library(gbm)
library(modelr)
library(car)

# TODO:
# 1. Figure out error for linear model
# 2. Add cross-validation
# 3. Implement KNN
# 4. Figure out performance issue with Boosting 
# 5. PCA?


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
