library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(forcats)
library(keras)


##### Import ############

df_2013 <- read_csv('data/Loans_20130101to20140101_20180415T060158.csv') %>% mutate(year = 2013)
df_2014 <- read_csv('data/Loans_20140101to20150101_20180415T060226.csv') %>% mutate(year = 2014)
df_2015 <- read_csv('data/Loans_20150101to20160101_20180415T060359.csv') %>% mutate(year = 2015)
df_2017 <- read_csv('data/Loans_20170101to20180101_20180415T060911.csv') %>% mutate(year = 2017)
df_2018 <- read_csv('data/Loans_20180101toCurrent_20180415T061114.csv') %>% mutate(year = 2018)


df_2018 <- df_2018 %>% mutate(loan_default_reason = as.integer(loan_default_reason))


df_all_years <- bind_rows(df_2013, df_2014, df_2015, df_2017, df_2018)
df_all_years_raw <- df_all_years

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


# Prep data
df_all_years_noNa <- drop_na(df_all_years) 

train_data <- sample_frac(df_all_years_noNa, .75)
test_data <- anti_join(df_all_years_noNa, train_data)

# Create train and test sets
x_train <- model.matrix(~ ., data = select(train_data, -lost_money))
y_train <- select(train_data, lost_money) %>% as.matrix()

x_test <- model.matrix(~ ., data = select(test_data, -lost_money))
y_test <- select(test_data, lost_money) %>% as.matrix()

# Standardize
train_mean <- apply(x_train, 2, mean)
train_std <- apply(x_train, 2, sd)

x_train <- scale(x_train, center = train_mean, scale = train_std)
x_test <- scale(x_test, center = train_mean, scale = train_std)

# Build model
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 512, activation = "relu", input_shape = dim(x_train)[[2]]) %>% 
    layer_dense(units = 512, activation = "relu") %>% 
    layer_dense(units = 512, activation = "relu") %>% 
    layer_dense(units = 512, activation = "relu") %>% 
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mse")
  )
}

k <- 10
num_epochs <- 1000
indices <- sample(1:nrow(x_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
all_mse_histories <- NULL


for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- x_train[val_indices,]
  val_targets <- y_train[val_indices]
  
  partial_train_data <- x_train[-val_indices,]
  partial_train_targets <- y_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  
  mse_history <- history$metrics$val_mean_squared_error
  all_mse_histories <- rbind(all_mse_histories, mse_history)
}

average_mse_history <- data.frame(
  epoch = seq(1:ncol(all_mse_histories)),
  validation_mse = apply(all_mse_histories, 2, mean)
)

write_csv(average_mse_history, 'data/average_mse_history')
