library(tidyverse)
library(MASS)

wheat_data <- read_tsv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/wheat/data sheet 1.csv")

wheat_part <- wheat_data[-c(1,2,4,35)]

# Prediction: train/test = 75/25 (235, 78)
train_lda_wheat <- sample(1:313, 235)
test_lda_wheat <- wheat_part[-train_lda_wheat,]
lda_Wheat <- lda(Growth ~ ., 
                 data = wheat_part,
                 prior = c(156,157)/313, 
                 subset = train_lda_wheat)

prediction_lda_Wheat <- predict(object = lda_Wheat,
                                newdata = test_lda_wheat)

tb_lda_wheat <- table(test_lda_wheat$Growth, prediction_lda_Wheat$class)
tb_lda_wheat

error_lda_wheat <- sum(tb_lda_wheat[row(tb_lda_wheat) != col(tb_lda_wheat)]) / sum(tb_lda_wheat)
accuracy_lda_wheat <- 1 - error_lda_wheat
accuracy_lda_wheat

