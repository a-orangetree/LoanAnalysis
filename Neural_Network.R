library(keras)

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

ggplot(average_mse_history, aes(x = epoch, y = validation_mse)) + geom_point()

model <- build_model()

model %>% fit(x_train, y_train,
              epochs = 60, batch_size = 1, verbose = 0)


result <- model %>% evaluate(x_test, y_test)
sqrt(result$mean_squared_error)