library(tidyverse)
library(biclust)
library(cvxbiclustr)

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

mango_part_bc <- mango_data[,-c(1,2)]

bc_mango_data <- discretize(mango_part_bc)
matrix_bc_mango_data <-  as.matrix(bc_mango_data)

# biclustering
bccc_mango <- biclust(matrix_bc_mango_data, method=BCCC(), alpha=1, delta = 0.3, number=5)
bccc_mango

parallelCoordinates( x= matrix_bc_mango_data, bicResult=bccc_mango, number=1)


bcbimax_mango <- biclust(matrix_bc_mango_data, method=BCBimax(), minr=2, minc=2, number=10)
bcbimax_mango

parallelCoordinates( x= matrix_bc_mango_data, bicResult=bcbimax_mango, number=1)


bcspectral_mango <- biclust(matrix_bc_mango_data, method=BCSpectral(), normalization="log", numberOfEigenvalues=3,
                         minr=2, minc=2, withinVar=1)
bcspectral_mango

parallelCoordinates( x= matrix_bc_mango_data, bicResult=bcspectral_mango, number=1)


bcxmotifs_mango <- biclust(matrix_bc_mango_data, method=BCXmotifs(), ns=20, nd=20, sd=5, alpha=0.01, number=10)
bcxmotifs_mango

parallelCoordinates( x= matrix_bc_mango_data, bicResult=bcxmotifs_mango, number=1)



# Convex biclustering


