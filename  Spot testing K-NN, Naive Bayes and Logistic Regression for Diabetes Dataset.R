# load libraries
library(mlbench)
library(caret)

# load data
data(PimaIndiansDiabetes)
# rename dataset to keep code below generic
dataset <- PimaIndiansDiabetes
head(dataset)


#data preprocessing
control <- trainControl(method="repeatedcv", number=13, repeats=3)
seed <- 5
metric<-"Accuracy"
preProcess=c("center", "scale")

# Logistic Regression model
set.seed(seed)
fit.glm <- train(diabetes~., data=dataset, method="glm", metric=metric, trControl=control)
fit.glm

#kNN model
set.seed(seed)
fit.knn <- train(diabetes~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
fit.knn


# Naive Bayes model
set.seed(seed)
fit.nb <- train(diabetes~., data=dataset, method="nb", metric=metric, trControl=control)
fit.nb


#listing all the model object together
results <- resamples(list( logistic=fit.glm,
                          knn=fit.knn, nb=fit.nb))

# Table comparison
summary(results)

# boxplot between different model
bwplot(results)

# Density Plot between different model
scales = list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

