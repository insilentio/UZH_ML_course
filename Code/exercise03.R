# 3.1
library(MASS)
str(crabs)

trainsample <- sample(1:nrow(crabs), .9*nrow(crabs))

knn.mod <- knn(train = crabs[4:8],
               test = crabs[4:8],
               cl = crabs$sp,
               k = 3)

confusionMatrix(knn.mod, crabs$sp)
crabs[crabs == trainsample]

# 3.2
taxi <- read.csv("Data/taxi03.csv", stringsAsFactors = TRUE)
str(taxi)
summary(taxi)

# b)
set.seed(1234)
train_sample <- sample(1:nrow(taxi), .9*nrow(taxi))
taxi_train <- taxi[train_sample,]
taxi_test <- taxi[-train_sample,]

taxi_knn <- knn(train = taxi_train[1:6],
                test = taxi_test[1:6],
                cl = taxi_train$col,
                k = 4)
ct <- confusionMatrix(taxi_knn, taxi_test$col)
ct$overall[1]


# c)
taxi <- data.frame(scale(taxi[1:6]), col = taxi$col)
train_sample <- sample(1:nrow(data), .9*nrow(data))

run_knn <- function(data, k) {
  taxi_train <- data[train_sample,]
  taxi_test <- data[-train_sample,]
  
  taxi_knn <- knn(train = taxi_train[1:6],
                  test = taxi_train[1:6],
                  cl = taxi_train$col,
                  k = k)
  ct <- confusionMatrix(taxi_knn, taxi_train$col)
  error_tr <- 1- ct$overall[1]
  
  taxi_knn <- knn(train = taxi_train[1:6],
                  test = taxi_test[1:6],
                  cl = taxi_train$col,
                  k = k)
  ct <- confusionMatrix(taxi_knn, taxi_test$col)
  error_te <- 1- ct$overall[1]
  return(tibble(train_error = error_tr, test_error = error_te))
}
taxis <- tibble(k = 1:30, train_error = NA, test_error = NA)
for (i in 1:30) {
  temp <- run_knn(taxi, i)
  taxis$train_error[i] = temp$train_error
  taxis$test_error[i] = temp$test_error
}


# d)
taxis <- taxis %>%
  pivot_longer(2:3, names_to = "traintest") %>%
  rename("error" = value)
ggplot(taxis) +
  aes(x = k, y = error, color = traintest) +
  geom_line()



