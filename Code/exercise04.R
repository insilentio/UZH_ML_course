taxi <- read.csv("Data/taxi03.csv", stringsAsFactors = TRUE)

# ex. 1
dat <- taxi[,-7]
label <- taxi$col
KNN_crossVal(data = dat, label = label, k_fold = 10, KNN_k = 5)

err <- c()
for (i in 1:10) {
  res <- KNN_crossVal(data = dat, label = label, k_fold = 10, KNN_k = i)
  err[i] <- res$errorRate
}
err

# ex. 2
library(caret)
### 10-Fold CV:
### Define tune grid:
t_grid <- data.frame(k=c(1:10, 10, 20, 50))   # Define k's for knn
### Define validation procedure:
train_crt <- trainControl(method = "cv", number = 10)
### "Train" the model:
set.seed(9823)
model <- train(col ~., data = taxi, method = "knn", tuneGrid=t_grid,
               trControl = train_crt, preProcess = "scale")
model
### Leave-one-out CV:
train_crt <- trainControl(method = "LOOCV")
model <- train(col ~., data = taxi, method = "knn", tuneGrid=t_grid,
               trControl = train_crt, preProcess = "scale")
model
### Repeated CV:
train_crt <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
model <- train(col ~., data = taxi, method = "knn", tuneGrid=t_grid,
               trControl = train_crt, preProcess = "scale")
model
