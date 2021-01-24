library(e1071)
data("iris")
mymodel <- svm(Species~., data = iris, kernel = "radial")
summary(mymodel)
plot(mymodel, data = iris,
    Sepal.Width~ Sepal.Length, slice = list(Petal.Length = 3, Petal.Width = 3))
#tunning
set.seed(123) 
tmodel <- tune(svm, Species~., data=iris,
     ranges = list(epsilon = seq(0,1, .1), cost = 2^(2:9)))
#higher cost will decrease number of Support vectors 
#which help to find better solution, but not to high to eliminate all potential vectors

plot(tmodel)
summary(tmodel)
mymodel <- tmodel$best.model

summary(mymodel)
pre <- predict(mymodel, iris)
t<- table(Predicted = pre, Actual = iris$Species)
t
1 - sum(diag(t))/sum(t) #accuracy of model
