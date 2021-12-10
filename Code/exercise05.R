library(party)

tree_iris <- ctree(Species ~., data = iris )
plot(tree_iris)
pred_tree <- predict(tree_iris, newdata = iris, type = "response")
confusionMatrix(pred_tree, iris$Species)


# a)
str(Pima.tr)

# b)
pima_tree <- ctree(type ~., Pima.tr)
pima_tree

# c)
plot(pima_tree)
# Since ctree() uses a significance test based method for variable selection, it compares
# the p-values (resulting from testing the relation between each predictor and the target
# variable statistically) between all predictors. The variable with the lowest p-value
# is selected for splitting. 

# d)
pred_pima_tr <- predict(pima_tree, newdata = Pima.tr, type = "response")
confusionMatrix(pred_pima_tr, Pima.tr$type)

pred_pima_te <- predict(pima_tree, newdata = Pima.te, type = "response")
confusionMatrix(pred_pima_te, Pima.te$type)

# e)
ctree_crossVal(Pima.tr[,-8], Pima.tr[,8], k_fold = 10)

### Repeated CV with caret:
### t_grid <- data.frame(k=c(1:10, 10, 20, 50))
train_crt <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
model <- train(type ~., data = Pima.tr, method = "ctree",
               trControl = train_crt, preProcess = "scale")
model

