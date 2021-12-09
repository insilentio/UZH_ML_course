# data
rw <- read.csv("Data/winequality-red.csv")
ww <- read.csv("Data/winequality-white.csv")

# a)
rw$colour <- "red"
ww$colour <- "white"
wines <- rbind(rw, ww)

# b)
wines$colour <- as.factor(wines$colour)
str(wines)

# c)
is.numeric(wines)
pc <- prcomp(wines[1:11], scale. = TRUE)
summary(pc)

#focus on first two PC
pc2 <- pc$x[,1:2]

# d) plot
ggplot(as.tibble(pc2)) +
  aes(x = PC1, y = PC2, colour = wines$colour) +
  geom_point()

# e) k-means
km_plot <- function(data, col=NULL, k) {
  km <- kmeans(data, centers = k)
  if (is.null(col)) col <- as.factor(km$cluster)
  ggplot(as.tibble(data)) +
    aes(x = PC1, y = PC2, colour = col) +
    geom_point()
}

set.seed(111)
km_plot(pc2, k = 2)

# f) again
set.seed(12)
km_plot(pc2, k = 2)

# g) 
km_plot(pc2, k=3)

# h)
# visually 2 clusters
wss <- tibble(clusters = 1:10, sos = NA)
for (i in 1:nrow(wss)) {
  wss$sos[i] <- kmeans(pc2, centers = i)$tot.withinss
}

ggplot(wss) +
  aes(x = clusters, y = sos) +
  geom_line()
# wss says 3

# ) i
distances <- dist(pc2)
hc <- hclust(distances, method = "ward.D2")
plot(hc, labels = FALSE)
ggplot(as.tibble(pc2)) +
  aes(x = PC1, y = PC2, colour = cutree(hc, k = 2)) +
  geom_point()


