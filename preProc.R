## load library
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(data.table)

## set global environment
options(stringsAsFactors = FALSE)
rm(list = ls())

## set wording directory
setwd("~/Workspace/shelter_animals/")
source("./lib.R")

## load data
animal_train <- fread("./data/train.csv", na.strings = "")
animal_test <- fread("./data/test.csv", na.strings = "")

## create features
train_feature <- animal_train %>% featEng()
train_target <- as.factor(animal_train$OutcomeType)
test_feature <- animal_test %>% featEng()

## convert character to factor
for(x in names(train_feature)) {
  if(class(train_feature[,x]) == "character") {
    cat("Converting", x, "to factor...\n")
    train_feature[,x] <- to_factor(train_feature[,x])
    test_feature[,x] <- to_factor(test_feature[,x], 
                                  levels = levels(train_feature[,x]))
  }
}

## create dummy variables
train_dummy <- dummyVars(~., train_feature)
train_dummy <- model.matrix(~. -1, data = train_feature, )

library(doMC)
registerDoMC(cores = detectCores())

## feature selection
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = TRUE)

lmProfile <- rfe(train_dummy,
                 train_target,
                 sizes = 1:length(train_dummy),
                 rfeControl = ctrl)




library(caret)
library(pROC)
set.seed(123)
inTraining <- createDataPartition(train_target, p = .8, list = FALSE)
subTrain <- train_feature[inTraining,]
subTest <- train_feature[-inTraining,]

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = multiClassSummary,
                           search = "random",
                           verboseIter = TRUE)

set.seed(825)
gbmFit <- train(x = subTrain, 
                y = train_target[inTraining],
                method = "gbm",
                metric = "ROC",
                tuneLength = 5,
                trControl = fitControl)

