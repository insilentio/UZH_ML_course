#' Partial Dependency Plot colored according to
#' Species in the iris data.


library(party)
cfor_ctr <- cforest_unbiased(ntree = 500, mtry = 2)
rf.SepWid <- cforest(Sepal.Width~., data = iris, controls = cfor_ctr)
steps <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
predic <- matrix(NA, nrow=nrow(iris), ncol = length(steps))
for (i in 1:nrow(iris)){
  obs <- iris[i,]
  obs$Sepal.Width <- NULL # Remove column
  for(s in 1:length(steps)){
    obs$Sepal.Length <- steps[s]
    predic[i,s] <- predict(rf.SepWid, newdata=obs)
  }
}
plot(NULL, xlim=c(min(iris$Sepal.Length), max(iris$Sepal.Length)),
     ylim=c(min(iris$Sepal.Width), max(iris$Sepal.Width)),
     xlab='Sepal.Length', ylab='Predicted Sepal.Width')
for(l in 1:nrow(predic)){
  lines(x = steps, y = predic[l,], col=as.numeric(iris$Species[l])+1)
}
lines(x=steps, y=apply(predic, 2, mean), col=1, lwd=6, lty=2)