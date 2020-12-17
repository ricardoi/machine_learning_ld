###############################################
#          SOURCE CODE FOR CHAPTER 19         #
###############################################

# LOADING PACKAGES ----
library(tidyverse)

# LOADING DATA ----
data(banknote, package = "mclust")

swissTib <- select(banknote, -Status) %>%
  as_tibble()

swissTib

# CLUSTERING WITH MCLUST ----
library(mclust)

swissMclust <- Mclust(swissTib)

plot(swissMclust)

# EXERCISES ----
# 1
swissMclust2 <- Mclust(swissTib, G = 2, modelNames = "VVE")

plot(swissMclust2)

# 2
mclustBoot2 <- clusterboot(swissTib, B = 10, 
                           clustermethod = noisemclustCBI,
                           G = 2, modelNames = "VVE",
                           showplots = FALSE)

mclustBoot3 <- clusterboot(swissTib, B = 10, 
                           clustermethod = noisemclustCBI,
                           G = 3, modelNames = "VVE",
                           showplots = FALSE)


mclustBoot2

mclustBoot3

# it can be challenging to compare the jaccard indices between models with
# different numbers of clusters. The model with three clusters may better
# represent nature, but as one of the clusters is small, the membership is
# more variable between bootstrap samples
