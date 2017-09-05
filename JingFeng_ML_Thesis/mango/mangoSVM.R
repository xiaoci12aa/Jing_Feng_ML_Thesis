library(tidyverse)
library(caret)
library(kernlab)

# library("e1071")

mango_data <- read_csv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/mango/mango.csv",
                       col_types = cols(
                         Sample = col_integer(),
                         Code = col_character(),
                         Fe = col_double(),
                         Mn = col_double(),
                         Ni = col_double(),
                         Cu = col_double(),
                         Co = col_double(),
                         Zn = col_double(),
                         Ca = col_double(),
                         Mg = col_double(),
                         Na = col_double(),
                         K = col_double()
                       ))

# check missing value
anyNA(mango_data)

# check the summarised details of data
summary(mango_data)

mango_part <- mango_data[,-1]

# attach(mango_data)
# x <- mango_data[, -c(1,2)]
# # Type of mango ( C / O)
# y <- mango_data[, 2]

# svm_model <- svm(Code ~ ., data = mango_part, type = "C")
# summary(svm_model)
# 
# # plot(svm_model, mango_part)
# 
# pred <- predict(svm_model,x)
# table1 <- table(pred, y)
# 
# error <- sum(table1[row(table1) != col(table1)]) / sum(table1)
# accuracy <- 1 - error
# accuracy


# svm_tune <- tune(svm, y ~ ., data = x, 
#                  kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)), type = "C")
# 
# print(svm_tune)

set.seed(1)

intrain_mango <- createDataPartition(y = mango_part$Code, p = 0.8, list = FALSE)
training_mango <- mango_part[intrain_mango,]
validation_mango <- mango_part[-intrain_mango,]


# check the correctness of partition
dim(training_mango)
dim(validation_mango)

trCtrl_mango <- trainControl(method = "LGOCV", number = 25, p = 0.8, returnResamp = "final",
                             verboseIter = TRUE)

set.seed(2)

mango_svm_polynomial <- train(Code ~., data = training_mango, method = 'svmPoly',
                              trControl = trCtrl_mango,
                              preProcess = c("center", "scale"),
                              metrics = "ROC",
                              tuneLength = 10)

mango_svm_polynomial

test_mango <- predict(mango_svm_polynomial, newdata = validation_mango)
test_mango

cf_svm_mango <- confusionMatrix(test_mango, validation_mango$Code)
cf_svm_mango
