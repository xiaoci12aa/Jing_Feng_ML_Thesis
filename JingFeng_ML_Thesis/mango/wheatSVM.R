library(tidyverse)
library(caret)
library(kernlab)

# filter data by year
wheat_data <- read_tsv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/wheat/data sheet 1.csv")
wheat2007 <- filter(wheat_data, Year == "y2007")
wheat2010 <- filter(wheat_data, Year == "y2010")

# FOR package e1071
# attach(wheat_data)
# x <- wheat_data[, -c(1:4, 35)]
# y <- wheat_data[, 3]
# 
# wheat_part <- wheat_data[, -c(1, 2, 4, 35)]
# svm_wheat <- svm(Growth ~ ., data = wheat_part, kernel = "polynomial", type = "C")
# summary(svm_wheat)
# 
# pred_wheat <- predict(svm_wheat,x)
# table2 <- table(pred_wheat, y)
# 
# error <- sum(table2[row(table2) != col(table2)]) / sum(table2)
# accuracy2 <- 1 - error
# accuracy2

# check missing value
anyNA(wheat_data)

# check the summarised details of data
summary(wheat_data)

# Data splitting
wheat_part <- wheat_data[, -c(1, 2, 4, 35)]
wheat_part2007 <- wheat2007[, -c(1, 2, 4, 35)]
wheat_part2010 <- wheat2010[, -c(1, 2, 4, 35)]

## Group 1: both trained and tested on year 2007
# Set seed to make work replicable
set.seed(30)

intrain2007 <- createDataPartition(y = wheat_part2007$Growth, p = 0.8, list = FALSE)
training2007 <- wheat_part2007[intrain2007,]
validation2007 <- wheat_part2007[-intrain2007,]

# check the correctness of partition
dim(training2007)
dim(validation2007)

# train SVM model, p for training percentage in LGOCV
trCtrl <- trainControl(method = "LGOCV", number = 25, p = 0.75, returnResamp = "final",
                       verboseIter = TRUE)

set.seed(31)
wheat_svm_polynomial2007 <- train(Growth ~., data = training2007, method = 'svmPoly',
                                  trControl = trCtrl,
                                  preProcess = c("center", "scale"),
                                  metrics = "ROC",
                                  tuneLength = 5)

wheat_svm_polynomial2007

test2007 <- predict(wheat_svm_polynomial2007, newdata = validation2007)
test2007

cm_svm2007 <- confusionMatrix(test2007, validation2007$Growth)
cm_svm2007

## Group 2: both trained and tested on year 2010
set.seed(30)

intrain2010 <- createDataPartition(y = wheat_part2010$Growth, p = 0.8, list = FALSE)
training2010 <- wheat_part2010[intrain2010,]
validation2010 <- wheat_part2010[-intrain2010,]

# check the correctness of partition
dim(training2010)
dim(validation2010)

set.seed(31)
wheat_svm_polynomial2010 <- train(Growth ~., data = training2010, method = 'svmPoly',
                                  trControl = trCtrl,
                                  preProcess = c("center", "scale"),
                                  metrics = "ROC",
                                  tuneLength = 5)

wheat_svm_polynomial2010

test2010 <- predict(wheat_svm_polynomial2010, newdata = validation2010)
test2010

cm_svm2010 <- confusionMatrix(test2010, validation2010$Growth)
cm_svm2010

## Group 3: trained on year 2007 and tested on year 2010

# check the correctness of partition
dim(training2007)
dim(wheat_part2010)

set.seed(31)
wheat_svm_polynomial2007_2010 <- train(Growth ~., data = training2007, method = 'svmPoly',
                                  trControl = trCtrl,
                                  preProcess = c("center", "scale"),
                                  metrics = "ROC",
                                  tuneLength = 5)

wheat_svm_polynomial2007_2010

test2007_2010 <- predict(wheat_svm_polynomial2007_2010, newdata = wheat_part2010)
test2007_2010

cm_svm2007_2010 <- confusionMatrix(test2007_2010, wheat_part2010$Growth)
cm_svm2007_2010

## Group 4: trained on year 2010 and tested on year 2007

# check the correctness of partition
dim(training2010)
dim(wheat_part2007)

set.seed(31)
wheat_svm_polynomial2010_2007 <- train(Growth ~., data = training2010, method = 'svmPoly',
                                       trControl = trCtrl,
                                       preProcess = c("center", "scale"),
                                       metrics = "ROC",
                                       tuneLength = 5)

wheat_svm_polynomial2010_2007

test2010_2007 <- predict(wheat_svm_polynomial2010_2007, newdata = wheat_part2007)
test2010_2007

cm_svm2010_2007 <- confusionMatrix(test2010_2007, wheat_part2007$Growth)
cm_svm2010_2007

## Group 5: both trained and tested on all three years
set.seed(30)

intrain <- createDataPartition(y = wheat_part$Growth, p = 0.8, list = FALSE)
training <- wheat_part[intrain,]
validation <- wheat_part[-intrain,]

# check the correctness of partition
dim(training)
dim(validation)

set.seed(31)
wheat_svm_polynomial <- train(Growth ~., data = training, method = 'svmPoly',
                                  trControl = trCtrl,
                                  preProcess = c("center", "scale"),
                                  metrics = "ROC",
                                  tuneLength = 5)

wheat_svm_polynomial

test <- predict(wheat_svm_polynomial, newdata = validation)
test

matrix_all <- confusionMatrix(test, validation$Growth)
matrix_all
