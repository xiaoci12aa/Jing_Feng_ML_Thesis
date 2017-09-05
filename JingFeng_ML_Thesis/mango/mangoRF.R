library(tidyverse)
library(caret)
library(randomForest)

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

mango_part <- mango_data[, -1]

# check missing value
anyNA(mango_part)

# check the summarised details of data
summary(mango_part)

# Set seed to make work replicable
set.seed(30)

intrainMango <- createDataPartition(y = mango_part$Code, p = 0.8, list = FALSE)
trainingMango <- mango_part[intrainMango,]
validationMango <- mango_part[-intrainMango,]

dim(trainingMango)
dim(validationMango)

# train RF model, p for training percentage in LGOCV
tc_mango <- trainControl(method = "LGOCV", number = 20, p = 0.75, returnResamp = "final",
                   verboseIter = TRUE)

set.seed(31)
mango_rf <- train(Code ~., data = trainingMango, method = 'rf',
                  trControl = tc_mango,
                  preProcess = c("center", "scale"),
                  metrics = "ROC",
                  tuneLength = 12,
                  importance = TRUE)

mango_rf

test_rf_mango <- predict(mango_rf, newdata = validationMango)
test_rf_mango

cm_rf_mango <- confusionMatrix(test_rf_mango, validationMango$Code)
cm_rf_mango
