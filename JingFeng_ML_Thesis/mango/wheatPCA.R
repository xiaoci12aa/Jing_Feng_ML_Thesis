library(tidyverse)
library(devtools)
library(ggord)
library(ggbiplot)
library(ggfortify)

wheat_data <- read_tsv(file = "/Users/XC/DOCUMENTS_COPY/UCL/Project/Final_Project/wheat/data sheet 1.csv")

# Option 1: Direct, except column 35: Ribitol, only metabolic profile
content.wheat <- wheat_data[, -c(1:4, 35)]

# Option 2: log, except column 35: Ribitol
# log.wheat <- log(wheat_data[, -c(1:4, 35)])

wheat.pca <- prcomp(content.wheat,
                   center = TRUE, 
                   scale. = TRUE)
print(wheat.pca)

prop.wheat.pca = wheat.pca$sdev^2/sum(wheat.pca$sdev^2)
print(prop.wheat.pca)

# plot for PC proportion, plot type: "l" for line, "b" for both
plot(prop.wheat.pca, type = "b", main = "", col = "blue",
     xlab = "PCs of Wheat Dataset", ylab = "Proportion of each PC",
     xlim = c(0, 36), ylim = c(0, 0.6))

summary_pca_wheat <- summary(wheat.pca)

# biplot 
print(biplot(wheat.pca, expand = 3, xlim = c(-0.1, 0.6), ylim = c(-0.3, 0.35)))

# pic 1
autoplot(wheat.pca, data = wheat_data, shape = 'Growth', colour = 'Variety') +
  labs(x = paste("PC1 (", percent(prop.wheat.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.wheat.pca[2]), ")", sep=""))

# circle each year
ggord(wheat.pca, wheat_data$Year, coord_fix = FALSE, txt = NULL, arrow = NULL)

# pic 2
wheat2007 <- filter(wheat_data, Year == "y2007")

content.wheat2007 <- wheat2007[, -c(1:4, 35)]

wheat2007.pca <- prcomp(content.wheat2007,
                        center = TRUE,
                        scale. = TRUE)

prop.wheat2007.pca <- wheat2007.pca$sdev^2/sum(wheat2007.pca$sdev^2)
print(prop.wheat2007.pca)
plot(wheat2007.pca, type = "l")

summary_pca_wheat2007 <- summary(wheat2007.pca)

print(biplot(wheat2007.pca))

autoplot(wheat2007.pca, data = wheat2007, shape = 'Growth', colour = 'Variety') +
  labs(x = paste("PC1 (", percent(prop.wheat2007.pca[1]), ")", sep = ""),
       y = paste("PC2 (", percent(prop.wheat2007.pca[2]), ")", sep = ""))

# pic 3
wheatRunal <- filter(wheat_data, Variety == "Runal")

content.wheatRunal <- wheatRunal[, -c(1:4, 35)]

wheatRunal.pca <- prcomp(content.wheatRunal,
                        center = TRUE,
                        scale. = TRUE)

prop.wheatRunal.pca <- wheatRunal.pca$sdev^2/sum(wheatRunal.pca$sdev^2)
print(prop.wheatRunal.pca)
plot(wheatRunal.pca, type = "b")

summary(wheatRunal.pca)

print(biplot(wheatRunal.pca))

autoplot(wheatRunal.pca, data = wheatRunal, shape = 'Growth', colour = 'Year') +
  labs(x = paste("PC1 (", percent(prop.wheatRunal.pca[1]), ")", sep = ""),
       y = paste("PC2 (", percent(prop.wheatRunal.pca[2]), ")", sep = ""))

# pic 4
autoplot(wheatRunal.pca, data = wheatRunal, shape = 'Growth', colour = 'Year') +
  labs(x = paste("PC2 (", percent(prop.wheatRunal.pca[2]), ")", sep = ""),
       y = paste("PC4 (", percent(prop.wheatRunal.pca[4]), ")", sep = ""))

