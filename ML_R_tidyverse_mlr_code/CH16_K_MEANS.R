###############################################
#          SOURCE CODE FOR CHAPTER 16         #
###############################################

# LOADING PACKAGES ----
library(mlr)

library(tidyverse)

# LOADING DATA ----
data(GvHD, package = "mclust")

gvhdTib <- as_tibble(GvHD.control)

gvhdTib

gvhdScaled <- gvhdTib %>% scale()

# PLOTTING THE DATA ----
library(GGally)

ggpairs(GvHD.control, 
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5)),
        diag = list(continuous = "densityDiag")) +
  theme_bw()

ggsave("gvhd.pdf", width = 10, height = 6)

# DEFINING THE TASK ----
gvhdTask <- makeClusterTask(data = as.data.frame(gvhdScaled))

# LISTING CLUSTERING LEARNERS IN MLR ----
listLearners("cluster")$class

# DEFINING THE K-MEANS LEARNER ----
kMeans <- makeLearner("cluster.kmeans", 
                      par.vals = list(iter.max = 100, nstart = 10))

# TUNING NUMBER OF CLUSTERS ----
getParamSet(kMeans)

kMeansParamSpace <- makeParamSet(
  makeDiscreteParam("centers", values = 3:8),
  makeDiscreteParam("algorithm", 
                    values = c("Hartigan-Wong", "Lloyd", "MacQueen")))

gridSearch <- makeTuneControlGrid()

kFold <- makeResampleDesc("CV", iters = 10)

#install.packages("clusterSim")

listMeasures("cluster")

tunedK <- tuneParams(kMeans, task = gvhdTask, 
                     resampling = kFold, 
                     par.set = kMeansParamSpace, 
                     control = gridSearch,
                     measures = list(db, dunn, G1))

tunedK

kMeansTuningData <- generateHyperParsEffectData(tunedK)

kMeansTuningData$data

gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric",
                             value = "Value",
                             c(-centers, -iteration, -algorithm))

ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw()

#ggsave("kmeanstuning.pdf", width = 10, height = 6)

# TRAINING FINAL MODEL WITH TUNED K ----
tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x)

tunedKMeansModel <- train(tunedKMeans, gvhdTask)

kMeansModelData <- getLearnerModel(tunedKMeansModel)

kMeansModelData

kMeansModelData$iter

# PLOTTING CLUSTERS ----
gvhdTib <- mutate(gvhdTib, 
                  kMeansCluster = as.factor(kMeansModelData$cluster))

ggpairs(gvhdTib, aes(col = kMeansCluster), 
        upper = list(continuous = "density"),
        lower = list(continuous = wrap("points", size = 0.5))) +
  theme_bw()

#ggsave("clusterPairs.pdf", width = 10, height = 6)

# PREDICTING NEW DATA ----
newCell <- tibble(CD4 = 510,
                  CD8b = 26,
                  CD3 = 500,
                  CD8 = 122) %>%
  scale(center = attr(gvhdScaled, "scaled:center"),
        scale = attr(gvhdScaled, "scaled:scale")) %>%
  as_tibble()

predict(tunedKMeansModel, newdata = newCell)

# EXERCISES ----
# 1
kMeans <- makeLearner("cluster.kmeans", 
                      par.vals = list(iter.max = 200, nstart = 10))

tunedK <- tuneParams(kMeans, task = gvhdTask, 
                     resampling = kFold, 
                     par.set = kMeansParamSpace, 
                     control = gridSearch,
                     measures = list(db, dunn, G1))

# the error about not converging disappears when we set iter.max to 200

# 2
gvhdPosTib <- as_tibble(GvHD.pos)

gvhdPosScaled <- scale(gvhdPosTib)

gvhdPosTask <- makeClusterTask(data = as.data.frame(gvhdPosScaled))

tunedKPos <- tuneParams(kMeans, task = gvhdPosTask, 
                        resampling = kFold, 
                        par.set = kMeansParamSpace, 
                        control = gridSearch,
                        measures = list(db, dunn, G1))

kMeansTuningDataPos <- generateHyperParsEffectData(tunedKPos)

gatheredTuningDataPos <- gather(kMeansTuningDataPos$data,
                                key = "Metric",
                                value = "Value",
                                c(-centers, -iteration, -algorithm))

ggplot(gatheredTuningDataPos, aes(centers, Value, col = algorithm)) +
  facet_wrap(~ Metric, scales = "free_y") +
  geom_line() +
  geom_point() +
  theme_bw()

tunedKMeansPos <- setHyperPars(kMeans, par.vals = list("centers" = 4))

tunedKMeansModelPos <- train(tunedKMeansPos, gvhdPosTask)

kMeansModelDataPos <- getLearnerModel(tunedKMeansModelPos)

mutate(gvhdPosTib, 
       kMeansCluster = as.factor(kMeansModelDataPos$cluster)) %>%
  ggpairs(mapping = aes(col = kMeansCluster), 
          upper = list(continuous = "density")) +
  theme_bw()

# the optimal number of clusters is less clear than for GvHD.control