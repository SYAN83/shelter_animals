library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

options(stringsAsFactors = FALSE)
## set wording directory
setwd("~/Workspace/shelterAnimals/")
rm(list = ls())
source("./lib.R")

## load data
animal_train <- fread("./data/train.csv", na.strings = "")
animal_test <- fread("./data/test.csv", na.strings = "")
train_feature <- animal_train %>% featEng()
train_target <- animal_train$OutcomeType
test_feature <- animal_test %>% featEng()


library(doMC)
library(parallel)
registerDoMC(cores = detectCores())

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

