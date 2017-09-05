library(tidyverse)
library(devtools)
library(ggord)
library(ggbiplot)
library(ggfortify)

# load data
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

# Option 1: Direct processing, only metabolic profile
content.mango <- mango_data[, -c(1,2)]

# Option 2: log transformation
# log.mango <- log(mango_data[, -c(1,2)])

# Type of mango ( C / O)
mango.code <- mango_data[, 2]

# apply PCA
mango.pca <- prcomp(content.mango,
                    center = TRUE,
                    scale. = TRUE)
print(mango.pca)

# proportion of each PC
prop.mango.pca <- mango.pca$sdev^2/sum(mango.pca$sdev^2)
print(prop.mango.pca)

plot(prop.mango.pca, type = "b", main = "", col = "blue",
     xlab = "PCs of Mango Dataset", ylab = "Proportion of each PC",
     xlim = c(0, 10), ylim = c(0, 0.2))

# # axis(side=1, at=c(0:23))
# axis(side=2, at=c(0:10))

summary_pca_mango <- summary(mango.pca)

# # data visulazation: bug!!!
# gMangoPCA <- ggbiplot(mango.pca, obs.scale = 1, var.scale = 1,
#                       groups = mango.code, ellipse = TRUE,
#                       circle = TRUE)
# gMangoPCA <- gMangoPCA + scale_color_discrete(name = '')
# gMangoPCA <- gMangoPCA + theme(legend.direction = 'horizontal',
#                                legend.position = 'top')

# biplot
print(biplot(mango.pca, expand = 2, xlim = c(-0.5, 0.5), ylim = c(-0.4, 0.4)))

# visualization
autoplot(mango.pca, data = mango_data, colour = 'Code') +
  labs(x = paste("PC1 (", percent(prop.mango.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.mango.pca[2]), ")", sep=""))

# circle not applied here because of mixture
ggord(mango.pca, mango.pca$Year, coord_fix = FALSE, txt = NULL, arrow = NULL)

