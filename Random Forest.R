df <- read.csv("direct_marketing.csv", header = T)
library(randomForest)
library(ggplot2)
library(cowplot)
View(df)
df <- transform(df, Age = as.factor(Age),
                Gender = as.factor(Gender), Married = as.factor(Married),
                Location = as.factor(Location), History = as.factor(History))
df$OwnHome <- as.factor(df$OwnHome)
str(df)

train <- df[1:750,]
test <- df[751:1000,]

dim(train)
dim(test)

set.seed(42)
#in case some data is missing in the training set we can use proximities to impute
data.imputed <- rfImpute(History~., data = df, iter = 6) #iteration should be within 4-6

rf <- randomForest(History~., data = train, ntree = 700, proximity = T)
rf

## Now check to see if the random forest is actually big enough...
## Up to a point, the more trees in the forest, the better. You can tell when
## you've made enough when the OOB no longer improves.
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf$err.rate), times=5), #times indicate number of levels in the variable History
  Type=rep(c("OOB", "High", "Low", "Medium", "None"), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"], 
          rf$err.rate[,"High"], 
          rf$err.rate[,"Low"],
          rf$err.rate[,"Medium"],
          rf$err.rate[,"None"]))
#plot error rate vs trees
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#comparing how many variables are effective
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(History ~ ., data=df, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1] #temp.model$err.rate is the last row of the OBB table which is also the result and colum 1 is the OBB value
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
rf <- randomForest(History ~ ., 
                      data=train,
                      ntree=700, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))

pred <- predict(rf, test)
cm <- table(test[,8], pred)
cm

#Not essential
## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=df$History)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
