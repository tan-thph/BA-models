library(tidyverse)
library(caret)
mark <- read.csv(file = "direct_marketing.csv", header = T)

mark$Age <- as.factor(mark$Age)
mark$Gender <- as.factor(mark$Gender)
mark$OwnHome <- as.factor(mark$OwnHome)
mark$Married <- as.factor(mark$Married)
mark$Location <- as.factor(mark$Location)
mark$History <- as.factor(mark$History)
str(mark)

#A good model will have a low value of the RMSE to increase the accuracy of the model.
#low value of MAE. R2 higher is better

#Validation Set Approach(or data split)
set.seed(123)
random_sample <- createDataPartition(mark$AmountSpent, 
                                     p = .8, list = F)
training_dataset <- mark[random_sample, ]
testing_dataset <- mark[-random_sample, ]
model <- lm(AmountSpent~., data = training_dataset)
summary(model)

library(GGally)
ggpairs(data = mark)

prediction <- predict(model, testing_dataset)

data.frame( R2 = R2(prediction, testing_dataset $ AmountSpent), 
            RMSE = RMSE(prediction, testing_dataset $ AmountSpent), 
            MAE = MAE(prediction, testing_dataset $ AmountSpent)) 

#k-fold cross validation 
set.seed(123)
train_control <- trainControl(method = "cv",
                             number = 10, repeats = 10)
model <- train(AmountSpent ~., data = mark,  
               method = "lm", 
               trControl = train_control) 
print(model)

