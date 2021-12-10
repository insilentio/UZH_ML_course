library(party)
library(pdp)

f_control <- cforest_unbiased(ntree = 500, mtry = 2)
rf_iris <- cforest(Species ~ ., iris, controls = f_control)
rf_iris
pred <- predict(rf_iris, OOB = TRUE)
confusionMatrix(pred, iris$Species)
varimp(rf_iris)

rf_sepwidth <- cforest(Sepal.Width ~., iris, controls = f_control)
partial(rf_sepwidth, pred.var = "Sepal.Length", plot = TRUE)


# ex 1 ----------------------------------------------------------------------------------------
# a)
rf_lm <- read.csv("Data/rf_lm_data.csv", stringsAsFactors = TRUE)
str(rf_lm)

# b)
set.seed(1241)
mysample <- sample(1:nrow(rf_lm), .7*nrow(rf_lm))
train <- rf_lm[mysample,]
test <- rf_lm[-mysample,]

# c)
reg <- lm(y ~., train)
summary(reg)

# d) 
f_control <- cforest_unbiased(ntree = 500, mtry = (length(rf_lm)-1)/3)
forest <- cforest(y ~ ., train, controls = f_control)

# e)
pred_lm <- predict(reg, newdata = test)
pred_rf <- predict(forest, newdata = test)
RMSE(pred_lm, test$y)
RMSE(pred_rf, test$y)

# f)
comb <- tibble(id = 1:nrow(test), real = test$y, pred_rf = pred_rf, pred_lm = pred_lm) %>%
  pivot_longer(3:4, "model")
ggplot(comb) +
  aes(value, real) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(aes(value, value), color = "red") +
  facet_grid(.~model)


# ex 2 ----------------------------------------------------------------------------------------
# a)
cust <- read.csv("Data/Wholesale_customers_data.csv")
str(cust)
cust$Channel <- as.factor(cust$Channel)
cust$Region <- as.factor(cust$Region)

# b)
set.seed(4321)
f_control <- cforest_unbiased(ntree = 500, mtry = 3)
forest <- cforest(Channel ~., cust, controls = f_control)
pred <- predict(forest, OOB = TRUE)  
confusionMatrix(pred, cust$Channel)  

# c)
set.seed(4321)
train_crt <- trainControl(method = "cv", number = 10)
cv_forest <- train(Channel ~., data = cust, method = "ctree", trControl = train_crt)
cv_forest

# d)
imp <- sort(varimp(forest), decreasing = TRUE)
imp

# e)
ggplot(cust) +
  aes(x = Channel, y = Detergents_Paper) +
  geom_boxplot()


# ex 3 ----------------------------------------------------------------------------------------
# a)
library(randomForest)
kaggle <- read_csv("Data/kaggle_train.csv")
str(kaggle)
kaggle$target <- as.factor(kaggle$target)

# b)
set.seed(123)
forest <- randomForest(target~., kaggle)
forest

cv_forest <- train(target ~., data = kaggle, method = "rpart", trControl = train_crt)
cv_forest


# c)
varImpPlot(forest)
