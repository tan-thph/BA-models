daf <- read.csv(file = "direct_marketing.csv", header = T)
library(class)
library(tidyverse)
library(ggplot2)
view(daf)
daf <- na.omit(daf)

library("cluster")
library("factoextra")
daf <- daf[,-1:-5]
daf <- daf[,-3]
daf <- scale(daf)

distance <- get_dist(daf)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k10 <- kmeans(daf, centers = 10, nstart = 25)
str(k10)
k2
fviz_cluster(k10, data = daf)

set.seed(123)
wss <- function(k){
  kmeans(daf, k, nstart = 10)$tot.withinss
}
k.values <- 1:15
wss_value <- map_dbl(k.values, wss)
plot(k.values, wss_value,
     type = "b", pch = 19, frame = F,
     xlab = "Number of cluster K",
     ylab = "Total within-clusters sum of squares")