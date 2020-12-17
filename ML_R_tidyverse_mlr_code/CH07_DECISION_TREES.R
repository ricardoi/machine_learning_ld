###############################################
#          SOURCE CODE FOR CHAPTER 7          #
###############################################

# LOAD PACKAGES ----
library(mlr)

library(tidyverse)

# LOAD DATA ----
data(Zoo, package = "mlbench")
zooTib <- as_tibble(Zoo)
zooTib

zooTib <- mutate_if(zooTib, is.logical, as.factor)
zooTib

# MAKE TASK AND LEARNER----
zooTask <- makeClassifTask(data = zooTib, target = "type")

tree <- makeLearner("classif.rpart")

# TUNE HYPERPARAMETERS ----
getParamSet(tree)

treeParamSpace <- makeParamSet(
  makeIntegerParam("minsplit", lower = 5, upper = 20),
  makeIntegerParam("minbucket", lower = 3, upper = 10),
  makeNumericParam("cp", lower = 0.01, upper = 0.1),
  makeIntegerParam("maxdepth", lower = 3, upper = 10))

randSearch <- makeTuneControlRandom(maxit = 200)

cvForTuning <- makeResampleDesc("CV", iters = 5)

library(parallel)
library(parallelMap)
 
parallelStartSocket(cpus = detectCores())

tunedTreePars <- tuneParams(tree, task = zooTask, # ~30 sec
                           resampling = cvForTuning, 
                           par.set = treeParamSpace, 
                           control = randSearch)

parallelStop()

tunedTreePars

tunedTreePars$x

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedTree <- setHyperPars(tree, par.vals = tunedTreePars$x)

tunedTreeModel <- train(tunedTree, zooTask)

# PLOTTING THE DECISION TREE ----
#install.packages("rpart.plot")
library(rpart.plot)

treeModelData <- getLearnerModel(tunedTreeModel)
rpart.plot(treeModelData, roundint = FALSE, type = 5)
printcp(treeModelData, digits = 3)
summary(treeModelData)

# INCLUDING HYPERPARAMETER TUNING INSIDE NESTED CROSS-VALIDATION ----
outer <- makeResampleDesc("CV", iters = 5)

treeWrapper <- makeTuneWrapper("classif.rpart", resampling = cvForTuning, 
                              par.set = treeParamSpace, 
                              control = randSearch) 

parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(treeWrapper, zooTask, resampling = outer) # ~2 min

parallelStop()

cvWithTuning