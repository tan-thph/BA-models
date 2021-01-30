#predict using a set of data
df <- read.csv(file = "EDSAL.csv", header = T)
library(tidyverse)
library(class)
library(ggplot2)
df <- df %>% arrange(Salary)
df <- as.data.frame(df)
a.lm <- lm(formula = Salary ~ Experience, data = df)
a.lm
new = data.frame(Experience=25)
predict(a.lm, new, interval = "predict")
