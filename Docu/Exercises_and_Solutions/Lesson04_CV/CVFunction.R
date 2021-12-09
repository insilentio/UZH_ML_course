# Date: 19-11-2021

#****************#
#   CVFunction   #
#****************#



#*********************************************************************************
#   CV FUNCTION   ####
#*********************************************************************************
KNN_crossVal <- function(data, label, k_fold=10, KNN_k=1){
  stopifnot(nrow(data)==length(label), is.factor(label), 
            (1<k_fold & k_fold<=nrow(data)), all(apply(data, 2, is.numeric)),
            KNN_k<=nrow(data))
  # Create k sub-selections
  n <- nrow(data)
  ind_s <- sample(1:n)
  ind.L <- list()
  j1 <- 1
  for (i in 1:k_fold){
    j2 <- (i*n) %/% k_fold
    ind.L[[i]] <- ind_s[j1:j2]
    j1 <- j2+1
  }
  # Now run KNN on each selection (and collect results):
  confMat <- matrix(0,nrow=nlevels(label), ncol=nlevels(label))
  for(fold in 1:k_fold){
    ind_fold <- ind.L[[fold]]
    testDat <- data[ind_fold,]
    trainDat <- data[-ind_fold,]
    test.solu <- label[ind_fold]
    train.solu <- label[-ind_fold]
    knn.pred <- class::knn(train = trainDat, test = testDat, 
                    cl = train.solu, k = KNN_k)
    confMat.fold <-  table(knn.pred, test.solu)
    confMat <- confMat + confMat.fold
  }
  # Calculate estimated test-error
  missMat <- confMat
  diag(missMat) <- 0
  missCount <- sum(missMat)
  test.err <- missCount/n
  L.res <- list(k_fold=k_fold, KNN_k=KNN_k, Indices=ind.L, 
                confMatrix=confMat, errorRate=test.err)
  return(L.res)
}
### Try out function:
dat <- iris[,-5]
label <- iris$Species
KNN_crossVal(data = dat, label = label, k_fold = 10, KNN_k = 5)


#*********************************************************************************
#   CARET EXAMPLES   ####
#*********************************************************************************
### Load package:
library(caret)
### 10-Fold CV:
### Define tune grid:
t_grid <- data.frame(k=c(1, 10, 50))   # Define k's for knn
### Define validation procedure:
train_crt <- trainControl(method = "cv", number = 10)
### "Train" the model:
set.seed(9823)
model <- train(Species ~., data = iris, method = "knn", tuneGrid=t_grid,
               trControl = train_crt)
model
### Leave-one-out CV:
train_crt <- trainControl(method = "LOOCV")
model <- train(Species ~., data = iris, method = "knn", tuneGrid=t_grid,
               trControl = train_crt)
model
### Repeated CV:
train_crt <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
model <- train(Species ~., data = iris, method = "knn", tuneGrid=t_grid,
               trControl = train_crt)
model
### CV for regression:
train_crt <- trainControl(method = "cv", number = 10)
model <- train(Sepal.Length ~., data = iris, method = "knn", tuneGrid=t_grid,
               trControl = train_crt)
model














