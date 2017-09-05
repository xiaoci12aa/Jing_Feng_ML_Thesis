library(tidyverse)
library(tsne)

wheat_data <- read_tsv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/wheat/data sheet 1.csv")

X_wheat <- wheat_data[,-c(1:4,35)]
# Y <- wheat_data[,3]

tsne_wheat <- tsne(X_wheat, perplexity = 30, k = 2, max_iter = 500)

# add necessary variebles to data frame
tsne_wheat_df <-  as.data.frame(tsne_wheat)
tsne_wheat_df$Year <- wheat_data$Year
tsne_wheat_df$Variety <- wheat_data$Variety
tsne_wheat_df$Growth <- wheat_data$Growth

# pic 1: tsne factor year & variety
ggplot(data = tsne_wheat_df) +
  geom_point(mapping = aes(x = V1, y = V2, shape = Year, colour = Variety))

# pic 2: tsne growth & variety
ggplot(data = tsne_wheat_df) +
  geom_point(mapping = aes(x = V1, y = V2, shape = Year, colour = Growth))

