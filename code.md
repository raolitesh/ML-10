# codes are written in R studio
```r
# installing and loading packages
install.packages("randomForest")
install.packages("caret", dependencies = TRUE)
install.packages("DMwR2")
install.packages("gmodels")
install.packages("pROC")
install.packages("dplyr")
install.packages("dismo")
library(randomForest)
library(caret)
library(gmodels)
library(pROC)
library(DMwR2)
library(dplyr)
library(dismo)
```
```r
# reading the data
company <- read.csv(file.choose())
attach(company)
summary(company
str(company)
names(company)
```
```r
# converting strings into categorical variables
company$ShelveLoc <- as.factor(company$ShelveLoc)
company$Urban <- as.factor(company$Urban)
company$US <- as.factor(company$US)
```
```r
# calculate SD, variance of a few features, correlation matrix, and draw a few plots
sd(Sales)
sd(CompPrice)
sd(Income)
var(Price)
var(Age)
var(Education)
cor(company[,-c(7,10,11)])
ggplot(company) + geom_histogram(aes(Sales),binwidth = 0.5, color = "cyan")
ggplot(company) + geom_histogram(aes(Income),binwidth = 1, color = "cyan")
```
```r
# converting the sales variable into 2 categories. If sales > 8, yes, else no.
High <- ifelse(Sales > 8,"Yes","No")
High <- as.factor(High)

# incorporating these two categories into data inplace of sales
company_new <- cbind(company[,-1],High) 
str(company_new)
prop.table(table(company_new$High))
```
```r
# training and testing data
indatapartition <- createDataPartition(company_new$High, p=.60, list = F) 
training <- company_new[indatapartition,]
testing <- company_new[-indatapartition,]
```
```r
# setting up control for training
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 15, verboseIter = TRUE, classProbs = TRUE)
```
```r
# executing the model
rf1 <- randomForest(High~. , data = company_new, ntree = 100, type = "class")
rf1
summary(rf1)
print(importance(rf1))
```
```r
# predicting the sales and checking the accuracy
pred1<- predict(rf1, company_new[,-11])
tab1 <- table(company_new[,11], pred1)
tab1
Acc1 <- sum(diag(tab1))/sum(tab1)
Acc1
```
```r
# setting up different number of trees along with their parameters
trees1 <- c(100,500,700,1100,1500)

acc <- c()
precision1 <- c()
recall1 <- c()
f1score <- c()
auc <- c()
```
```r
# executing the model with these trees
for(i in trees1) {
  print(i)
  
  forest1 <- randomForest(High~. , data = training, trControl = ctrl,method = "gbm", ntree = i)
  forest1
  pred <- predict(forest1,testing[,-11], type = "response")
  conf <- confusionMatrix(pred,testing$High, mode = "everything")
  conf$byClass
  areaundercurve <- roc(response = testing$High, predictor =factor(pred, ordered = TRUE), decreasing = TRUE)
  acc <- c(acc, conf$overall[1])
  precision1 <- c(precision1, conf$byClass[5])
  recall1 <- c(recall1, conf$byClass[6])
  f1score <- c(f1score, conf$byClass[7])
  auc <- c(auc, areaundercurve$auc)
}
```
```r
# checking their parameter's value for model fit
recall1
precision1
acc
```
```r
# overall evaluation
Evaluation <-data.frame("No of Trees" = trees1, "Accuracy" = acc, "Precision" = precision1, "Recall" = recall1, "F1" = f1score, "AUC" = auc)
Evaluation
```







