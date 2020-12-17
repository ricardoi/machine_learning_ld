###############################################
#          SOURCE CODE FOR CHAPTER 17         #
###############################################

# LOADING PACKAGES ----
library(tidyverse)

# LOADING DATA ----
data(GvHD, package = "mclust")

gvhdTib <- as_tibble(GvHD.control)

gvhdTib

gvhdScaled <- gvhdTib %>% scale()

# CREATING DISTANCE MATRIX ----
gvhdDist <- dist(gvhdScaled, method = "euclidean")

# RUNNING CLUSTERING ----
gvhdHclust <- hclust(gvhdDist, method = "ward.D2")

# PLOTTING DENDROGRAM ----
gvhdDend <- as.dendrogram(gvhdHclust)

plot(gvhdDend, leaflab = "none")

# DEFINE CLUSTERING METRIC FUNCTION ----
cluster_metrics <- function(data, clusters, dist_matrix) {
  list(db   = clusterSim::index.DB(data, clusters)$DB,
       G1   = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters = length(unique(clusters))
  )
}

# GENERATE BOOTSTRAP SAMPLES OF DATA ----
gvhdBoot <- map(1:10, ~ {
  gvhdScaled %>%
    as_tibble() %>%
    sample_n(size = nrow(.), replace = TRUE)
})

# CLUSTER AND CALCULATE METRICS FOR EACH BOOTSTRAP ----
metricsTib <- map_df(gvhdBoot, function(boot) {
  d <- dist(boot, method = "euclidean")
  cl <- hclust(d, method = "ward.D2")
  
  map_df(3:8, function(k) {
    cut <- cutree(cl, k = k)
    cluster_metrics(boot, clusters = cut, dist_matrix = d)
  })
})

metricsTib

# MUTATE A BOOTSTRAP COLUMN AND GATHER ----
metricsTib <- metricsTib %>%
  mutate(bootstrap = factor(rep(1:10, each = 6))) %>%
  gather(key = "Metric", value = "Value", -clusters, -bootstrap)

# PLOT THE BOOTSTRAP RESULTS ----
ggplot(metricsTib, aes(as.factor(clusters), Value)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line(size = 0.1, aes(group = bootstrap)) +
  geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
  stat_summary(fun.data="mean_cl_boot",  
               geom="crossbar", width = 0.5, fill = "white") +
  theme_bw()

#ggsave("hclustBootstrap.pdf", width = 10, height = 5)

# CUTTING DENDROGRAM ----
gvhdCut <- cutree(gvhdHclust, k = 4)

plot(gvhdDend, leaflab = "none")

rect.hclust(gvhdHclust, k = 4)

# PLOTTING CLUSTERS ----
gvhdTib <- mutate(gvhdTib, hclustCluster = as.factor(gvhdCut))

ggpairs(gvhdTib, aes(col = hclustCluster), 
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5))) +
  theme_bw()

#ggsave("hclusterPairs.pdf", width = 10, height = 6)

# CALCULATE JACCARD'S DISTANCE BETWEEN BOOTSTRAP SAMPLES ----
library(fpc)

par(mfrow = c(3, 4))

clustBoot <- clusterboot(gvhdDist, B = 10, 
                         clustermethod = disthclustCBI,
                         k = 4, method = "ward.D2",
                         showplots = TRUE)

clustBoot

# EXERCISES ----
# 1
gvhdDistMan <- dist(gvhdScaled, method = "manhattan")

gvhdHclustMan <- hclust(gvhdDistMan, method = "ward.D2")

gvhdDendMan <- as.dendrogram(gvhdHclustMan)

plot(gvhdDendMan, leaflab = "none")

# 2
group_by(metricsTib, Metric) %>%
  mutate(Value = scale(Value)) %>%
  group_by(Metric, clusters) %>%
  mutate(Stdev = sd(Value)) %>%
  
  ggplot(aes(as.factor(clusters), Metric, fill = Value, height = Stdev)) +
  geom_tile() +
  theme_bw() +
  theme(panel.grid = element_blank())

# 3
par(mfrow = c(3, 4))

clustBoot <- clusterboot(gvhdScaled,
                         B = 10,
                         clustermethod = kmeansCBI,
                         k = 4, algorithm = "Lloyd",
                         showplots = TRUE)

clustBoot

# k-means seems to give more stable clusters

# 4
library(cluster)

gvhdDiana <- as_tibble(gvhdScaled) %>% diana()

as.dendrogram(gvhdDiana) %>% plot(leaflab = "none")

# 5
cluster_metrics <- function(data, clusters, dist_matrix, linkage) {
  list(db   = clusterSim::index.DB(data, clusters)$DB,
       G1   = clusterSim::index.G1(data, clusters),
       dunn = clValid::dunn(dist_matrix, clusters),
       clusters = length(unique(clusters)),
       linkage = linkage
  )
}

metricsTib <- map_df(gvhdBoot, function(boot) {
  d <- dist(boot, method = "euclidean")
  linkage <- c("ward.D2", "single", "complete", "average", "centroid")
  
  map_df(linkage, function(linkage) {
    cl <- hclust(d, method = linkage)
    cut <- cutree(cl, k = 4)
    cluster_metrics(boot, clusters = cut, dist_matrix = d, linkage)
  })
})

metricsTib

metricsTib <- metricsTib %>%
  mutate(bootstrap = factor(rep(1:10, each = 5))) %>%
  gather(key = "Metric", value = "Value", -clusters, -bootstrap, -linkage)

ggplot(metricsTib, aes(linkage, Value)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line(size = 0.1, aes(group = bootstrap)) +
  geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
  stat_summary(fun.data="mean_cl_boot",  
               geom="crossbar", width = 0.5, fill = "white") +
  theme_bw()

# single linkage seems the best indicated by DB and Dunn,
# though pseudo F disagrees

# 6
gvhdHclustSingle <- hclust(gvhdDist, method = "single")

gvhdCutSingle <- cutree(gvhdHclustSingle, k = 4)

gvhdTib <- mutate(gvhdTib, gvhdCutSingle = as.factor(gvhdCutSingle))

select(gvhdTib, -hclustCluster) %>%
  ggpairs(aes(col = gvhdCutSingle), 
          upper = list(continuous = "density"),
          lower = list(continuous = wrap("points", size = 0.5))) +
  theme_bw()

# using single linkage on this dataset does a terrible job of finding
# clusters! This is why visual evaluation of clusters is important,
# don't blindly rely on internal metrics only!