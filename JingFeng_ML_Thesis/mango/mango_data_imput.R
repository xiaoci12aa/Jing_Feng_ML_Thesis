library(tidyverse)
require(MASS)

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

train <- sample(1:130, 65)
ldaM2 <- lda(Code ~ .,data = mango_part,prior = c(7,6)/13, subset = train)
plda2 <- predict(object = ldaM2,
                 newdata = mango_data[-train,])


# head(plda2$x,3)
# 
# ldaMango <- lda(Code ~ .,mango_data[,-1],prior = c(7,6)/13, CV = TRUE)
# ldaMango
# prop.ldaMango = ldaMango$sdev^2/sum(ldaMango$sdev^2)
# print(prop.ldaMango)

# prediction accuracy
tt <- table(mango_part$Code, plda2$class)

error <- sum(tt[row(tt) != col(tt)]) / sum(tt)
accuracy <- 1 - error
accuracy
