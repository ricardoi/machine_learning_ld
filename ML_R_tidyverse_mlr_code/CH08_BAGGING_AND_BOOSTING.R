###############################################
#          SOURCE CODE FOR CHAPTER 8          #
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

forest <- makeLearner("classif.randomForest")

# TUNE HYPERPARAMETERS ----
getParamSet(forest)

forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 300, upper = 300),
  makeIntegerParam("mtry", lower = 6, upper = 12),
  makeIntegerParam("nodesize", lower = 1, upper = 5),
  makeIntegerParam("maxnodes", lower = 5, upper = 20))

randSearch <- makeTuneControlRandom(maxit = 100)

cvForTuning <- makeResampleDesc("CV", iters = 5)

library(parallelMap)
library(parallel)

parallelStartSocket(cpus = detectCores())

tunedForestPars <- tuneParams(forest, task = zooTask, # ~20 sec
                              resampling = cvForTuning, 
                              par.set = forestParamSpace, 
                              control = randSearch)

parallelStop()

tunedForestPars

tunedForestPars$x

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedForest <- setHyperPars(forest, par.vals = tunedForestPars$x)

tunedForestModel <- train(tunedForest, zooTask)

forestModelData <- getLearnerModel(tunedForestModel)

species <- colnames(forestModelData$err.rate)

plot(forestModelData, col = 1:length(species), lty = 1:length(species))

legend("topright", species, 
       col = 1:length(species),
       lty = 1:length(species))

# INCLUDING HYPERPARAMETER TUNING INSIDE NESTED CROSS-VALIDATION ----
outer <- makeResampleDesc("CV", iters = 5)

forestWrapper <- makeTuneWrapper("classif.randomForest", resampling = cvForTuning, 
                                 par.set = forestParamSpace, 
                                 control = randSearch) 

parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(forestWrapper, zooTask, resampling = outer) # ~1 min

parallelStop()

cvWithTuning

# MAKE XGBOOST LEARNER ----
xgb <- makeLearner("classif.xgboost")

# RESTRUCTURE TIBBLE FOR XGBOOST ----
zooXgb <- mutate_at(zooTib, .vars = vars(-type), .funs = as.numeric)

xgbTask <- makeClassifTask(data = zooXgb, target = "type")

# TUNE HYPERPARAMETERS ----
getParamSet(xgb)

xgbParamSpace <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 5),
  makeIntegerParam("max_depth", lower = 1, 5),
  makeNumericParam("min_child_weight", lower = 1, upper = 10),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeIntegerParam("nrounds", lower = 20, upper = 20),
  makeDiscreteParam("eval_metric", values = c("merror", "mlogloss")))

randSearch <- makeTuneControlRandom(maxit = 1000)

cvForTuning <- makeResampleDesc("CV", iters = 5)

tunedXgbPars <- tuneParams(xgb, task = xgbTask, # ~3 min
                           resampling = cvForTuning, 
                           par.set = xgbParamSpace, 
                           control = randSearch)

tunedXgbPars

tunedXgbPars$x

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedXgb <- setHyperPars(xgb, par.vals = tunedXgbPars$x)

tunedXgbModel <- train(tunedXgb, xgbTask)

# PLOTTING LOSS AND TREE STRUCTURE ----
xgbModelData <- getLearnerModel(tunedXgbModel)

ggplot(xgbModelData$evaluation_log, aes(iter, train_mlogloss)) +
  geom_line() +
  geom_point() +
  theme_bw()

#ggsave("logloss.pdf", width = 5, height = 3)
install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

indivTrees <- xgboost::xgb.plot.tree(model = xgbModelData, trees = 1:10, render = F)
DiagrammeR::export_graph(indivTrees, "indivTrees.pdf")

# INCLUDING HYPERPARAMETER TUNING INSIDE NESTED CROSS-VALIDATION ----
outer <- makeResampleDesc("CV", iters = 3)

xgbWrapper <- makeTuneWrapper("classif.xgboost", resampling = cvForTuning,
                              par.set = xgbParamSpace, 
                              control = randSearch) 

cvWithTuning <- resample(xgbWrapper, xgbTask, resampling = outer)

cvWithTuning

# BENCHMARKING LEARNERS AGAINST EACH OTHER ----
# If you don't have tunedTree still defined in your global environment,
# re-run the code from the previous chapter on decision trees

learners = list(makeLearner("classif.knn"), 
                makeLearner("classif.LiblineaRL1LogReg"),
                makeLearner("classif.svm"),
                tunedTree,
                tunedForest,
                tunedXgb)

benchCV <- makeResampleDesc("RepCV", folds = 10, reps = 5)

bench <- benchmark(learners, xgbTask, benchCV)

bench
