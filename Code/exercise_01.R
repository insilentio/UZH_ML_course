library("MASS")
library("class")
library("caret")
library("party")
library("randomForest")
library("nnet")
library(tidyverse)

# a)
cancer <- read.csv("Data/breastCancer_Wisconsin.csv", stringsAsFactors=TRUE)
cancer$id <- as.factor(cancer$id)

# b) 
str(cancer)
dim(cancer)

# c)
pc <- prcomp(cancer[-c(1:2)], scale. = TRUE)
summary(pc)
# d)
# 44% by first PC, 79% by first 4

# e)
pc$rotation[which.max(abs(pc$rotation[,1])), 1]

# f) coordinates of first imaage
pc$x[1,1:3]

# g)
ggplot(as.tibble(pc$x)) +
  aes(x = PC1, y = PC2, colour = cancer$diagnosis) +
  geom_point()

# h) no scaliing
pc_wo_sc <- prcomp(cancer[-c(1:2)], scale. = FALSE)
ggplot(as.tibble(pc_wo_sc$x)) +
  aes(x = PC1, y = PC2, colour = cancer$diagnosis) +
  geom_point()

# i) screeplot
screeplot(pc, npcs = 30, type = "l")
# -> 3 to 7

# j)
pc5 <- prcomp(cancer[c(3:7)], scale. = TRUE)
biplot(pc5)
# -> smoothness mean