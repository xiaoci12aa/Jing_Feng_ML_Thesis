library(tidyverse)
library(tsne)

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

X_mango <- mango_data[,-c(1,2)]

# grid search
tsne_mango <- tsne(X_mango, perplexity = 100, k = 2, max_iter = 500)

tsne_mango_df <- as.data.frame(tsne_mango)
tsne_mango_df$Code <- mango_data$Code

ggplot(data = tsne_mango_df) +
  geom_point(mapping = aes(x = V1, y = V2, colour = Code))

