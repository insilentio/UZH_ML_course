library(nnet)

# only one hidden layer possible with this package
# 

nn1 <- nnet(Sepal.Length ~ scale(Sepal.Width) + scale(Petal.Length) + scale(Petal.Width),
            data = iris,
            size=2,
            decay=0,
            linout=TRUE,
            maxit=10000)
summary(nn1)
predict(nn1, newdata = iris)

nn2 <- nnet(Species ~ Sepal.Length + scale(Sepal.Width) + scale(Petal.Length) + scale(Petal.Width),
            data = iris,
            size=2,
            decay=0,
            linout=FALSE,
            maxit=10000)
summary(nn2)
predict(nn2, newdata = iris)


tune <- expand.grid(size = c(1, 5, 10), decay = c(0, .5))
model <- train(Sepal.Length ~., data = iris, method = "nnet", tuneGrid = tune, maxit = 1e4,
                linout = TRUE, preProcess = c("center", "scale"))
model


# ex 1 ----------------------------------------------------------------------------------------
# a)
library(MASS)
str(Boston)
summary(Boston)

# b)
Boston_scaled <- Boston %>%
  # mutate(chas = as.factor(chas)) %>%
  mutate(across(c(1:13), scale))
summary(Boston_scaled)

# c)
nnBoston <- nnet(medv~ .,
                 data = Boston_scaled, size = 1, decay = 0, maxit = 1e4, linout = TRUE)
nnBoston
pred <- predict(nnBoston, Boston_scaled)
confusionMatrix(as.vector(pred), Boston_scaled$medv)

# d)
tune <- expand.grid(size = c(1:5, 10, 20), decay = 0)

# e)
model <- train(medv~., Boston_scaled, method = "nnet", tuneGrid = tune, maxit = 1e4, linout = TRUE)
model

# f)
set.seed(7349)
f_control <- cforest_unbiased(ntree = 500, mtry = 5)
forest <- cforest(medv~., Boston_scaled, controls = f_control)
pred_rf <- predict(forest, OOB = TRUE)
sqrt(mean((Boston_scaled$medv - pred_rf)^2))


# ex 2 ----------------------------------------------------------------------------------------
# a)
test_array <- array(1:24, 2:4)

# b)
test_array[2, 3, 1]
test_array[,,2]

# c)
load("Data/Mnist_training_and_test_data.RData")
str(train_x.0)
str(test_x.0)

# d)
train_x.0[1,28,2]

# e)
image(train_x.0[,,18])

# f)
### Turn into 2d matrices (each row one picture):
train_x <- array(as.numeric(aperm(train_x.0, perm = c(3, 1, 2))), dim = c(60000, 784))
test_x <- array(as.numeric(aperm(test_x.0, perm = c(3, 1, 2))), dim = c(10000, 784))
### Scale to 0-1 range:
train_x <- train_x/255
test_x <- test_x/255
### Turn number labels to factor:
train_y <- as.factor(train_y.0)
test_y <- as.factor(test_y.0)
### Combine to dataframe:
mnist_train <- data.frame("y"=train_y, train_x)
mnist_test <- data.frame("y"=test_y, test_x)
### Look at dimensions:
dim(mnist_train)
dim(mnist_test)

# g)
load("Data/NN_mnist.RData")
nne.mnist <- nnet(y ~., mnist_train, size = 20, decay = 0, maxit = 1e3, MaxNWts = 16000)

# h)
pred_mnist_nnet <- predict(nne.mnist, mnist_test, type = "class")
confusionMatrix(as.factor(pred_mnist_nnet), mnist_test$y)

# i)
forest.mnist <- randomForest(y~., mnist_train, ntree = 50)
pred_mnist_forest <- predict(forest.mnist, mnist_test)
confusionMatrix(pred_mnist_forest, mnist_test$y)

