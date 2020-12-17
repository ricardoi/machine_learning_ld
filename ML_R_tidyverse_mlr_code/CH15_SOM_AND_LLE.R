###############################################
#          SOURCE CODE FOR CHAPTER 15         #
###############################################

# LOADING TIDYVERSE AND GGALLY ----
library(tidyverse)

library(GGally)

# LOAD AND PLOT DATA ----
data(flea)

fleaTib <- as_tibble(flea)

fleaTib

ggpairs(flea, mapping = aes(col = species)) + 
  theme_bw()

#ggsave("flea.pdf", width = 10, height = 6)

# TRAINING THE SOM ----
#install.packages("kohonen")

library(kohonen)

#set.seed(24601) # to reproduce exactly the same embedding as me

somGrid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal", 
                   neighbourhood.fct = "bubble", toroidal = FALSE)

fleaScaled <- fleaTib %>% 
  select(-species) %>%
  scale()

fleaSom <- som(fleaScaled, grid = somGrid, rlen = 5000, 
               alpha = c(0.05, 0.01))

# PLOT SOM RESULTS ----
par(mfrow = c(2, 3))

plotTypes <- c("codes", "changes", "counts", "quality", 
               "dist.neighbours", "mapping")

walk(plotTypes, ~plot(fleaSom, type = ., shape = "straight"))

# PLOT INDIVIDUAL VARIABLES ON SOM MAP ----
getCodes(fleaSom) %>%
  as_tibble() %>%
  iwalk(~plot(fleaSom, type = "property", property = ., 
             main = .y, shape = "straight"))

# PLOTTING GROUPS ON SOM MAP ----
par(mfrow = c(1, 2))

nodeCols <- c("cyan3", "yellow", "purple")

plot(fleaSom, type = "mapping", pch = 21, 
     bg = nodeCols[as.numeric(fleaTib$species)],
     shape = "straight", bgcol = "lightgrey")

# CLUSTER AND ADD TO SOM MAP ----
clusters <- cutree(hclust(dist(fleaSom$codes[[1]], 
                               method = "manhattan")), 3)

somClusters <- map_dbl(clusters, ~{
  if(. == 1) 3
  else if(. == 2) 2
  else 1
}
)

plot(fleaSom, type = "mapping", pch = 21, 
     bg = nodeCols[as.numeric(fleaTib$species)],
     shape = "straight",
     bgcol = nodeCols[as.integer(somClusters)])

add.cluster.boundaries(fleaSom, somClusters)

par(mfrow = c(1, 1))

# MAPPING NEW DATA ONTO SOM ----
newData <- tibble(tars1 = c(120, 200),
                  tars2 = c(125, 120),
                  head = c(52, 48),
                  aede1 = c(140, 128),
                  aede2 = c(12, 14),
                  aede3 = c(100, 85)) %>%
           scale(center = attr(fleaScaled, "scaled:center"),
                 scale = attr(fleaScaled, "scaled:scale"))

predicted <- predict(fleaSom, newData)

plot(fleaSom, type = "mapping", classif = predicted, shape = "round")

# LOCALLY-LINEAR EMBEDDING ----
#install.packages("lle")

library(lle)

data(lle_scurve_data)

colnames(lle_scurve_data) <- c("x", "y", "z")

sTib <- as_tibble(lle_scurve_data)

sTib

# PLOT DATA IN 3D ----
#install.packages(c("plot3D", "plot3Drgl"))
library(plot3D)

scatter3D(x = sTib$x, y = sTib$y, z = sTib$z, pch = 19,
          bty = "b2", colkey = FALSE, theta = 35, phi = 10,
          col = ramp.col(c("darkred", "lightblue")))

plot3Drgl::plotrgl()

# PERFORM LLE EMBEDDING ON SCURVE ----
lleK <- calc_k(lle_scurve_data, m = 2, kmin = 1, kmax = 20, 
               parallel = TRUE, cpus = parallel::detectCores())

lleBestK <- filter(lleK, rho == min(lleK$rho))

lleBestK

lleCurve <- lle(lle_scurve_data, m = 2, k = lleBestK$k)

sTib <- sTib %>%
  mutate(LLE1 = lleCurve$Y[, 1],
         LLE2 = lleCurve$Y[, 2])

ggplot(sTib, aes(LLE1, LLE2, col = z)) +
  geom_point() +
  scale_color_gradient(low = "darkred", high = "lightblue") +
  theme_bw()
#ggsave("2dLLE.pdf", width = 10, height  = 5)

# PERFORM LLE EMBEDDING ON FLEA DATA ----
lleFleaK <- calc_k(fleaScaled, m = 2, kmin = 1, kmax = 20,
                   parallel = TRUE, cpus = parallel::detectCores())

lleBestFleaK <- filter(lleFleaK, rho == min(lleFleaK$rho))

lleBestFleaK

lleFlea <- lle(fleaScaled, m = 2, k = lleBestFleaK$k)

fleaTib <- fleaTib %>%
  mutate(LLE1 = lleFlea$Y[, 1],
         LLE2 = lleFlea$Y[, 2])

ggplot(fleaTib, aes(LLE1, LLE2, col = species)) +
  geom_point() +
  theme_bw()

#ggsave("LLEflea.pdf", width = 10, height = 4)

# EXERCISES ----
# 1
somGridRect <- somgrid(xdim = 5, ydim = 5, topo = "rectangular", 
                   toroidal = TRUE)

fleaSomRect <- som(fleaScaled, grid = somGridRect, rlen = 5000,
                   alpha = c(0.05, 0.01))

plot(fleaSomRect, type = "mapping", pch = 21, 
     bg = nodeCols[as.numeric(fleaTib$species)],
     shape = "straight", bgcol = "lightgrey")

# making the map toroidal means that nodes on one edge are connected to
# adjacent nodes on the opposite side of the map

# 2
ggplot(fleaTib, aes(LLE1, LLE2, col = species)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()

# 3
fleaSomAlpha <- som(fleaScaled, grid = somGrid, rlen = 10000,
                   alpha = c(0.01, 0.001))

plot(fleaSomAlpha, type = "mapping", pch = 21, 
     bg = nodeCols[as.numeric(fleaTib$species)],
     shape = "straight", bgcol = "lightgrey")

# while the positions of the groups change between repeats, there is less
# variation in how well cases from the same species cluster together.
# This is because the learning rate is slower and there are more iterations

# 4
lleFlea3 <- lle(fleaScaled, m = 3, k = lleBestFleaK$k)

fleaTib <- fleaTib %>%
  mutate(LLE1 = lleFlea3$Y[, 1],
         LLE2 = lleFlea3$Y[, 2],
         LLE3 = lleFlea3$Y[, 3])

scatter3D(x = fleaTib$LLE1, y = fleaTib$LLE2, z = fleaTib$LLE3, pch = 19, 
          bty = "b2", colkey = FALSE, theta = 35, phi = 10, cex = 2,
          col = c("red", "blue", "green")[as.integer(fleaTib$species)],
          ticktype = "detailed")

plot3Drgl::plotrgl()

# 5
lleFleaUnscaled <- lle(dplyr::select(fleaTib, -species), 
                       m = 2, 
                       k = lleBestFleaK$k)

fleaTib <- fleaTib %>%
  mutate(LLE1 = lleFleaUnscaled$Y[, 1],
         LLE2 = lleFleaUnscaled$Y[, 2])

ggplot(fleaTib, aes(LLE1, LLE2, col = species)) +
  geom_point() +
  theme_bw()

# as we can see, the embedding is different depending on whether
# the variables are scaled or not