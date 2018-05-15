library(keras)

x_train <- model.matrix(~ ., data = select(drop_na(df_all_years), -lost_money)
y_train <- select(drop_na(df_all_years), lost_money) %>% as.matrix()

train_mean <- apply(x_train, 2, mean)
train_std <- apply(x_train, 2, sd)

x_train <- scale(x_train, center = train_mean, scale = train_std)

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", 
                input_shape = dim(x_train)[[2]]) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = "mse", 
    metrics = c("mae")
  )
}

k <- 2
indices <- sample(1:nrow(x_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
num_epochs <- 5
all_scores <- c()

for (i in 1:k) {
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- x_train[val_indices,]
  val_targets <- y_train[val_indices]
  
  partial_train_data <- x_train[-val_indices,]
  partial_train_targets <- y_train[-val_indices]
  
  model <- build_model()
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 1, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$mean_absolute_error)
}

mean(all_scores)