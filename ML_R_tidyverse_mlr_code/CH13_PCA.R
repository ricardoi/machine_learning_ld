###############################################
#          SOURCE CODE FOR CHAPTER 13         #
###############################################

# LOADING TIDYVERSE ----
library(tidyverse)

# LOADING DATA ----
data(banknote, package = "mclust")

swissTib <- as_tibble(banknote)

swissTib

# PLOTTING DATA ----
#install.packages("GGally")

library(GGally)

ggpairs(swissTib, mapping = aes(col = Status)) + 
  theme_bw()
#ggsave("Swiss.pdf", width = 10, height = 5)

# PRINCIPAL COMPONENTS ANALYSIS ----
pca <- select(swissTib, -Status) %>%
  prcomp(center = TRUE, scale = TRUE)

pca

summary(pca)

map_dfc(1:6, ~pca$rotation[, .] * sqrt(pca$sdev ^ 2)[.])

# PLOTTING PCA RESULT ----
#install.packages("factoextra")

library(factoextra)

pcaDat <- get_pca(pca)

pcaDat$coord

fviz_pca_biplot(pca, label = "var")
#ggsave("biplot.pdf", width = 5, height = 5)

fviz_pca_var(pca)
#ggsave("loadings.pdf", width = 5, height = 5)

fviz_screeplot(pca, addlabels = TRUE, choice = "eigenvalue")
#ggsave("eigen.pdf", width = 5, height = 5)

fviz_screeplot(pca, addlabels = TRUE, choice = "variance")
#ggsave("percent.pdf", width = 5, height = 5)

swissPca <- swissTib %>%
  mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])

ggplot(swissPca, aes(PCA1, PCA2, col = Status)) +
  geom_point() +
  theme_bw()

#ggsave("finalPCA.pdf", width = 7, height = 5)

# COMPUTING COMPONENT SCORES FOR NEW DATA ----
newBanknotes <- tibble(
  Length = c(214, 216), 
  Left = c(130, 128),
  Right = c(132, 129), 
  Bottom = c(12, 7),
  Top = c(12, 8),
  Diagonal = c(138, 142)
)

predict(pca, newBanknotes)

# SOLUTIONS TO EXERCISES ----
# 1
ggplot(swissPca, aes(PCA1, PCA2, col = Status)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()

# 2
pcaUnscaled <- select(swissTib, -Status) %>%
  prcomp(center = TRUE, scale = FALSE)

pcaUnscaled

fviz_pca_biplot(pcaUnscaled, label = "var")

fviz_pca_var(pcaUnscaled)

fviz_screeplot(pcaUnscaled, addlabels = TRUE, choice = "variance")

# 3
pcaUncentered <- select(swissTib, -Status) %>%
  prcomp(center = FALSE, scale = TRUE)

pcaUncentered

fviz_pca_biplot(pcaUncentered, label = "var")

fviz_pca_var(pcaUncentered)

fviz_screeplot(pcaUncentered, addlabels = TRUE, choice = "variance")