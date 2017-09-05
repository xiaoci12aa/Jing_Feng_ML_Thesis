library(tidyverse)
library(MASS)

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

mango_part <- mango_data[,-1]

# Prediction: train/test = 75/25 (98, 32)
train_lda_mango <- sample(1:130, 98)
test_lda_mango <- mango_part[-train_lda_mango,]
lda_Mango <- lda(Code ~ ., data = mango_part,
                 prior = c(7,6)/13, 
                 subset = train_lda_mango)

prediction_lda_Mango <- predict(object = lda_Mango,
                 newdata = test_lda_mango)


# head(plda2$x,3)
# 
# ldaMango <- lda(Code ~ .,mango_data[,-1],prior = c(7,6)/13, CV = TRUE)
# ldaMango
# prop.ldaMango = ldaMango$sdev^2/sum(ldaMango$sdev^2)
# print(prop.ldaMango)

# prediction accuracy
tb_lda_mango <- table(test_lda_mango$Code, prediction_lda_Mango$class)

error_lda_mango <- sum(tb_lda_mango[row(tb_lda_mango) != col(tb_lda_mango)]) / sum(tb_lda_mango)
accuracy_lda_mango <- 1 - error_lda_mango
accuracy_lda_mango
