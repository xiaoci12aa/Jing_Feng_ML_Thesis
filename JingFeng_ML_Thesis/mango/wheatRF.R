library(tidyverse)
library(caret)
library(randomForest)

# filter data by year
wheat_data <- read_tsv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/wheat/data sheet 1.csv")
wheat2007 <- filter(wheat_data, Year == "y2007")
wheat2009 <- filter(wheat_data, Year == "y2009")
wheat2010 <- filter(wheat_data, Year == "y2010")

# check missing value
anyNA(wheat2009)

# check the summarised details of data
summary(wheat2009)

# Data splitting
wheat_part <- wheat_data[, -c(1, 2, 4, 35)]
wheat_part2007 <- wheat2007[, -c(1, 2, 4, 35)]
# wheat_part2009 <- wheat2009[, -c(1, 2, 4, 35)]
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

# train RF model, p for training percentage in LGOCV
tc <- trainControl(method = "LGOCV", number = 20, p = 0.75, returnResamp = "final",
                       verboseIter = TRUE)

set.seed(31)
wheat_rf2007 <- train(Growth ~., data = training2007, method = 'rf',
                                  trControl = tc,
                                  preProcess = c("center", "scale"),
                                  metrics = "ROC",
                                  tuneLength = 12,
                                  importance = TRUE)

wheat_rf2007

test_rf2007 <- predict(wheat_rf2007, newdata = validation2007)
test_rf2007

cm_rf2007 <- confusionMatrix(test_rf2007, validation2007$Growth)
cm_rf2007


# ## Group 2: both trained and tested on year 2009 <- meaningless, too small the subset is
# # Set seed to make work replicable
# set.seed(30)
# 
# intrain2009 <- createDataPartition(y = wheat_part2009$Growth, p = 0.8, list = FALSE)
# training2009 <- wheat_part2009[intrain2009,]
# validation2009 <- wheat_part2009[-intrain2009,]
# 
# # check the correctness of partition
# dim(training2009)
# dim(validation2009)
# 
# set.seed(31)
# wheat_rf2009 <- train(Growth ~., data = training2009, method = 'rf',
#                       trControl = tc,
#                       preProcess = c("center", "scale"),
#                       metrics = "ROC",
#                       tuneLength = 12,
#                       importance = TRUE)
# 
# wheat_rf2009
# 
# test_rf2009 <- predict(wheat_rf2009, newdata = validation2009)
# test_rf2009
# 
# cm_rf2009 <- confusionMatrix(test_rf2009, validation2009$Growth)
# cm_rf2009


## Group 2: both trained and tested on year 2010
# Set seed to make work replicable
set.seed(30)

intrain2010 <- createDataPartition(y = wheat_part2010$Growth, p = 0.8, list = FALSE)
training2010 <- wheat_part2010[intrain2010,]
validation2010 <- wheat_part2010[-intrain2010,]

# check the correctness of partition
dim(training2010)
dim(validation2010)

set.seed(31)
wheat_rf2010 <- train(Growth ~., data = training2010, method = 'rf',
                      trControl = tc,
                      preProcess = c("center", "scale"),
                      metrics = "ROC",
                      tuneLength = 12,
                      importance = TRUE)

wheat_rf2010

test_rf2010 <- predict(wheat_rf2010, newdata = validation2010)
test_rf2010

cm_rf2010 <- confusionMatrix(test_rf2010, validation2010$Growth)
cm_rf2010


## Group 3: both trained and tested on all three years
set.seed(30)

intrain <- createDataPartition(y = wheat_part$Growth, p = 0.8, list = FALSE)
training <- wheat_part[intrain,]
validation <- wheat_part[-intrain,]

# check the correctness of partition
dim(training)
dim(validation)

set.seed(31)
wheat_rf <- train(Growth ~., data = training, method = 'rf',
                      trControl = tc,
                      preProcess = c("center", "scale"),
                      metrics = "ROC",
                      tuneLength = 12,
                      importance = TRUE)

wheat_rf

test_rf <- predict(wheat_rf, newdata = validation)
test_rf

cm_rf <- confusionMatrix(test_rf, validation$Growth)
cm_rf

# group 4:07-10
set.seed(31)
wheat_rf2007_2010 <- train(Growth ~., data = training2007, method = 'rf',
                      trControl = tc,
                      preProcess = c("center", "scale"),
                      metrics = "ROC",
                      tuneLength = 12,
                      importance = TRUE)

wheat_rf2007_2010

test_rf2007_2010 <- predict(wheat_rf2007_2010, newdata = wheat_part2010)
test_rf2007_2010

cm_rf2007_2010 <- confusionMatrix(test_rf2007_2010, wheat_part2010$Growth)
cm_rf2007_2010

# group 5: 10-07
set.seed(31)
wheat_rf2010_2007 <- train(Growth ~., data = training2010, method = 'rf',
                      trControl = tc,
                      preProcess = c("center", "scale"),
                      metrics = "ROC",
                      tuneLength = 12,
                      importance = TRUE)

wheat_rf2010_2007

test_rf2010_2007 <- predict(wheat_rf2010_2007, newdata = wheat_part2007)
test_rf2010_2007

cm_rf2010_2007 <- confusionMatrix(test_rf2010_2007, wheat_part2007$Growth)
cm_rf2010_2007

