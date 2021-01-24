library(e1071)
library(class)
data("iris")
data_norm <- function(x){((x - min(x))/(max(x) - min(x)))} #normlising data due to different scale
iris_norm <- as.data.frame(lapply(iris[,-5], data_norm))#apply function to data set without colum 5
summary(iris)
summary(iris_norm)
train <- iris_norm[1:100,]
test <- iris_norm[101:150,]
pred <- knn(train, test, iris[1:100,5], k = 12)#iris[1:100,5] means predict the value of colum 5
table(pred, iris[101:150,5])  
