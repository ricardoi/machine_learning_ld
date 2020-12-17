###############################################
#          SOURCE CODE FOR CHAPTER 18         #
###############################################

# LOADING PACKAGES ----
library(tidyverse)

# LOADING DATA ----
data(banknote, package = "mclust")

swissTib <- select(banknote, -Status) %>%
  as_tibble()

swissTib

swissScaled <- swissTib %>% scale()

# PLOTTING DATA ----
library(GGally)

ggpairs(swissTib, upper = list(continuous = "density")) + 
  theme_bw()
#ggsave("SwissDBSCAN.pdf", width = 10, height = 5)

# SELECTING RANGE OF EPS VALUES FOR TUNING ----
library(dbscan)

kNNdistplot(swissScaled, k = 5)

abline(h = c(1.2, 2.0))

# MANUALLY CREATING HYPERPARAMETER SEARCH SPACE ----
dbsParamSpace <- expand.grid(eps = seq(1.2, 2.0, 0.1),
                             minPts = seq(1, 9, 1))

dbsParamSpace

# PERFORM DBSCAN ----
swissDbs <- pmap(dbsParamSpace, dbscan, x = swissScaled)

swissDbs[[5]]

clusterResults <- map_df(swissDbs, ~.$cluster)

clusterResults

swissClusters <- bind_cols(swissTib, clusterResults) # sounds like a breakfast cereal

swissClusters

swissClustersGathered <- gather(swissClusters, 
                                key = "Permutation", value = "Cluster",
                                -Length, -Left, -Right, 
                                -Bottom, -Top, -Diagonal)

swissClustersGathered

# PLOT CLUSTERING OVER TWO VARIABLES ----
ggplot(swissClustersGathered, aes(Right, Diagonal, 
                                  col = as.factor(Cluster))) +
  facet_wrap(~ Permutation) +
  geom_point(size = 0.7) +
  theme_bw() +
  theme(legend.position = "none")

#ggsave("DBSCANTuning.pdf", width = 10, height = 10)

# DEFINE CLUSTERING METRIC FUNCTION ----
cluster_metrics <- function(data, clusters, dist_matrix) {
  list(db   = clusterSim::index.DB(data, clusters)$DB,
       G1   = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters = length(unique(clusters))
  )
}

# GENERATE BOOTSTRAP SAMPLES OF DATA ----
swissBoot <- map(1:10, ~ {
  swissScaled %>%
    as_tibble() %>%
    sample_n(size = nrow(.), replace = TRUE)
})

# CLUSTER AND CALCULATE METRICS FOR EACH BOOTSTRAP ----
metricsTib <- map_df(swissBoot, function(boot) {
  clusterResult <- pmap(dbsParamSpace, dbscan, x = boot)
  
  map_df(clusterResult, function(permutation) {
    clust <- as_tibble(permutation$cluster)
    filteredData <- bind_cols(boot, clust) %>%
      filter(value != 0)
    
    d <- dist(select(filteredData, -value))
    
    cluster_metrics(select(filteredData, -value), 
                    clusters = filteredData$value, 
                    dist_matrix = d)
  })
})

# MUTATE A BOOTSTRAP COLUMN AND GATHER ----
metricsTibSummary <- metricsTib %>%
  mutate(bootstrap = factor(rep(1:10, each = 81)),
         eps = factor(rep(dbsParamSpace$eps, times = 10)),
         minPts = factor(rep(dbsParamSpace$minPts, times = 10))) %>%
  
  gather(key = "metric", value = "value", 
         -bootstrap, -eps, -minPts) %>%
  
  mutate_if(is.numeric, ~ na_if(., Inf)) %>%
  drop_na() %>%
  group_by(metric, eps, minPts) %>%
  summarize(meanValue = mean(value),
            num = n()) %>%
  group_by(metric) %>%
  mutate(meanValue = scale(meanValue)) %>%
  ungroup()

# PLOT THE BOOTSTRAP RESULTS ----
ggplot(metricsTibSummary, aes(eps, minPts, 
                              fill = meanValue, alpha = num)) +
  facet_wrap(~ metric) +
  geom_tile(col = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank())

#ggsave("dbscanTuningPlot.pdf", width = 10, height = 6)

# PLOT THE PERMUTATION WITH THE BEST BOOTSTRAP RESULT ----
which(dbsParamSpace$eps == 1.2 & dbsParamSpace$minPts == 9)

## WITH OUTLIERS
filter(swissClustersGathered, Permutation == "V73") %>%
  select(-Permutation) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  ggpairs(mapping = aes(col = Cluster), 
          upper = list(continuous = "density")) + 
  theme_bw()

#ggsave("dbscanWithOutliers.pdf", width = 10, height = 6)

## WITHOUT OUTLIERS
filter(swissClustersGathered, Permutation == "V73", Cluster != 0) %>%
  select(-Permutation) %>%
  mutate(Cluster = as.factor(Cluster)) %>%
  ggpairs(mapping = aes(col = Cluster), 
          upper = list(continuous = "density")) + 
  theme_bw()

#ggsave("dbscanWithoutOutliers.pdf", width = 10, height = 6)

# BOOTSTRAPPING THE JACCARD INDEX ----

library(fpc)

clustBoot <- clusterboot(swissScaled, B = 500, 
                         clustermethod = dbscanCBI,
                         eps = 1.2, MinPts = 9,
                         showplots = FALSE)

clustBoot

# CLUSTERING WITH OPTICS ----
swissOptics <- optics(swissScaled, minPts = 9)

plot(swissOptics)

swissOpticsXi <- extractXi(swissOptics, xi = 0.05)

# PLOTTING OPTICS CLUSTERS ----
swissTib %>% 
  mutate(cluster = factor(swissOpticsXi$cluster)) %>%
  ggpairs(mapping = aes(col = cluster),
          upper = list(continuous = "points")) +
  theme_bw()

#ggsave("opticsClusters.pdf", width = 10, height = 6)

# EXERCISES ----
# 1
dbsParamSpace

# the function creates a data frame whose rows make up
# every combination of the input vectors

# 2
ggplot(swissClustersGathered, aes(reorder(Permutation, Cluster), 
             fill = as.factor(Cluster))) +
  geom_bar(position = "fill", col = "black") +
  theme_bw() +
  theme(legend.position = "none")

ggplot(swissClustersGathered, aes(reorder(Permutation, Cluster), 
                                  fill = as.factor(Cluster))) +
  geom_bar(position = "fill", col = "black") +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none")

ggplot(swissClustersGathered, aes(Permutation, 
                                  fill = as.factor(Cluster))) +
  geom_bar(position = "fill", col = "black") +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none")

# the reorder function orders the levels of the first argument, 
# according to the values of the second argumnt

# 3
swissDbsNoOutlier <- dbscan::dbscan(swissScaled, eps = 1.2, minPts = 1)

swissDbsNoOutlier

# there are no cases in the noise cluster because the minimum cluster
# size is now 1, meaning all cases are core points

# 4
swissDbsUnscaled <- dbscan::dbscan(swissTib, eps = 1.2, minPts = 9)

swissDbsUnscaled

# the clusters are not the same as those learned for the scaled data.
# This is because DBSCAN and OPTICS are sensitive to scale differences

# 5
swissOpticsXi035 <- extractXi(swissOptics, xi = 0.035)
plot(swissOpticsXi035)

swissOpticsXi05 <- extractXi(swissOptics, xi = 0.05)
plot(swissOpticsXi05)

swissOpticsXi065 <- extractXi(swissOptics, xi = 0.065)
plot(swissOpticsXi065)

# EXAMPLE CLUSTERING IN FIGURE 1 ----
library(dbscan)
library(tidyverse)

data("DS3")

ds3DbCluster <- dbscan(DS3, eps = 10, minPts = 20)
ds3KmeansCluster <- kmeans(DS3, centers = 6)
ds3HclustCluster <- hclust(dist(DS3))

DS3$dbscan <- factor(ds3DbCluster$cluster)
DS3$kmeans <- factor(ds3KmeansCluster$cluster)
DS3$hclust <- factor(cutree(ds3HclustCluster, k = 6))

gather(DS3, key = "algorithm", value = "cluster", -X, -Y) %>%
  ggplot(aes(x = X, y = Y, col = cluster)) +
  facet_wrap(~ algorithm) +
  geom_point() +
  theme_bw()

#ggsave("comparingDBSCAN.pdf", width = 10, height = 3.5)
