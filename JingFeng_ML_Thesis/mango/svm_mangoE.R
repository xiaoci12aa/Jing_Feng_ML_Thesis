library(tidyverse)
library("e1071")

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

# attach(mango_data)
mango_part <- mango_data[,-1]

# Data splitting
smp_size <- floor(0.8 * nrow(mango_part))

set.seed(773)
train_index <- sample(seq_len(nrow(mango_part)), size = smp_size)
training_svm_mango <- mango_part[train_index,]
validation_svm_mango <- mango_part[-train_index,]

svm_mango <- svm(Code ~ ., data = training_svm_mango, type = "C", kernel = "radial")
summary(svm_model)

# plot(svm_model, mango_part)

pred_mango <- predict(svm_mango, newdata = validation_svm_mango)
tb_svm_mango <- table(validation_svm_mango$Code, pred_mango)
tb_svm_mango

error_svm_mango <- sum(tb_svm_mango[row(tb_svm_mango) != col(tb_svm_mango)]) / sum(tb_svm_mango)
accuracy_svm_mango <- 1 - error_svm_mango
accuracy_svm_mango

svm_tune <- tune(svm, Code ~ ., data = mango_part, 
                 ranges=list(cost=2^(2:4), gamma=2^(-1:1)),
                 tunecontrol = tune.control(sampling = "fix"),
                 type = "C" 
                 )

print(svm_tune)

