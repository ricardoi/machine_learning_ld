###############################################
#          SOURCE CODE FOR CHAPTER 12         #
###############################################

# LOADING PACKAGES ----
library(mlr)

library(tidyverse)

# LOADING DATA ----
data("fuelsubset.task")

fuel <- getTaskData(fuelsubset.task)

fuelTib <- as_tibble(fuel)

fuelTib

# PLOTTING THE DATA ----
fuelUntidy <- fuelTib %>%
  mutate(id = 1:nrow(.)) %>% 
  gather(key = "variable", value = "absorbance", 
         c(-heatan, -h20, -id)) %>%
  mutate(spectrum = str_sub(variable, 1, 3),
         wavelength = as.numeric(str_extract(variable, "\\d+")))

fuelUntidy %>%
  ggplot(aes(absorbance, heatan, col = as.factor(wavelength))) +
  facet_wrap(~ spectrum, scales = "free_x") +
  geom_smooth(se = FALSE, size = 0.2) +
  ggtitle("Absorbance vs heatan for each wavelength") +
  theme_bw() +
  theme(legend.position = "none")

#ggsave("Spectra.pdf", width = 10, height = 5)

fuelUntidy %>%
  ggplot(aes(wavelength, absorbance, group = id, col = heatan)) +
  facet_wrap(~ spectrum, scales = "free_x") +
  geom_smooth(se = FALSE, size = 0.2) +
  ggtitle("Wavelength vs absorbance for each batch") +
  theme_bw()

#ggsave("Wavelength.pdf", width = 10, height = 5)

fuelUntidy %>%
  ggplot(aes(h20, heatan)) +
  geom_smooth(se = FALSE) +
  ggtitle("Humidity vs heatan") +
  theme_bw()

#ggsave("H20.pdf", width = 10, height = 5)

# (RE)DEFINING THE TASK ----
fuelTask <- makeRegrTask(data = fuelTib, target = "heatan")

# DEFINING THE K-NN LEARNER ----
kknn <- makeLearner("regr.kknn")

getParamSet(kknn)

kknnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:12))

gridSearch <- makeTuneControlGrid()

kFold <- makeResampleDesc("CV", iters = 10)

tunedK <- tuneParams(kknn, task = fuelTask, 
                     resampling = kFold, 
                     par.set = kknnParamSpace, 
                     control = gridSearch)

tunedK

knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mse.test.mean",
                    plot.type = "line") +
  theme_bw()

#ggsave("Tuning kknn.pdf", width = 10, height = 5)

# TRAINING FINAL MODEL WITH TUNED K ----
tunedKnn <- setHyperPars(makeLearner("regr.kknn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, fuelTask)

# DEFINING THE RANDOM FOREST LEARNER ----
forest <- makeLearner("regr.randomForest")

forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 50, upper = 50),
  makeIntegerParam("mtry", lower = 100, upper = 366),
  makeIntegerParam("nodesize", lower = 1, upper = 10),
  makeIntegerParam("maxnodes", lower = 5, upper = 30))

randSearch <- makeTuneControlRandom(maxit = 100)

library(parallel)

library(parallelMap)

parallelStartSocket(cpus = detectCores())

tunedForestPars <- tuneParams(forest, task = fuelTask, # ~2 min
                              resampling = kFold, 
                              par.set = forestParamSpace, 
                              control = randSearch)

parallelStop()

tunedForestPars

# TRAINING FINAL RANDOM FOREST MODEL WITH TUNED HYPERPARAMETERS ----
tunedForest <- setHyperPars(forest, par.vals = tunedForestPars$x)

tunedForestModel <- train(tunedForest, fuelTask)

forestModelData <- getLearnerModel(tunedForestModel)

plot(forestModelData)

# MAKE XGBOOST LEARNER ----
xgb <- makeLearner("regr.xgboost")

# TUNE HYPERPARAMETERS ----
getParamSet(xgb)

xgbParamSpace <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 10),
  makeIntegerParam("max_depth", lower = 1, upper = 20),
  makeNumericParam("min_child_weight", lower = 1, upper = 10),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeIntegerParam("nrounds", lower = 30, upper = 30))

tunedXgbPars <- tuneParams(xgb, task = fuelTask, #~1.5 min
                           resampling = kFold, 
                           par.set = xgbParamSpace, 
                           control = randSearch)

tunedXgbPars

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedXgb <- setHyperPars(xgb, par.vals = tunedXgbPars$x)

tunedXgbModel <- train(tunedXgb, fuelTask)

# PLOTTING RMSE ----
xgbModelData <- getLearnerModel(tunedXgbModel)

ggplot(xgbModelData$evaluation_log, aes(iter, train_rmse)) +
  geom_line() +
  geom_point() +
  theme_bw()

#ggsave("rmse.pdf", width = 10, height = 5)

# BENCHMARKING MODELS AGAINST EACH OTHER ----
# MAKE TUNING WRAPPERS

kknnWrapper <- makeTuneWrapper(kknn, resampling = kFold,
                                par.set = kknnParamSpace, 
                                control = gridSearch) 

forestWrapper <- makeTuneWrapper(forest, resampling = kFold,
                                par.set = forestParamSpace, 
                                control = randSearch) 

xgbWrapper <- makeTuneWrapper(xgb, resampling = kFold,
                                  par.set = xgbParamSpace, 
                                  control = randSearch) 

learners = list(kknnWrapper, forestWrapper, xgbWrapper)

holdout <- makeResampleDesc("Holdout")

bench <- benchmark(learners, fuelTask, holdout) # ~ 7 min

bench

# EXERCISES ----
# 1
fuelUntidy %>%
  ggplot(aes(absorbance, heatan, col = as.factor(wavelength))) +
  facet_wrap(~ spectrum, scales = "free_x") +
  geom_smooth(se = FALSE, size = 0.2) +
  geom_smooth(group = 1, col = "blue") +
  ggtitle("Absorbance vs heatan for each wavelength") +
  theme_bw() +
  theme(legend.position = "none")

# 2
kknnParamSpace50 <- makeParamSet(makeDiscreteParam("k", values = 1:50))

tunedK50 <- tuneParams(kknn, task = fuelTask, 
                     resampling = kFold, 
                     par.set = kknnParamSpace50, 
                     control = gridSearch)

tunedK50

knnTuningData50 <- generateHyperParsEffectData(tunedK50)

plotHyperParsEffect(knnTuningData50, x = "k", y = "mse.test.mean",
                    plot.type = "line") +
  theme_bw()

# our original search space was large enough

# 3
benchKFold <- benchmark(learners, fuelTask, kFold) 

plotBMRBoxplots(benchKFold)

#4
holdout <- makeResampleDesc("Holdout")

randSearch2000 <- makeTuneControlRandom(maxit = 2000)

forestWrapper2000 <- makeTuneWrapper(forest, resampling = holdout,
                                     par.set = forestParamSpace,
                                     control = randSearch2000)

parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(forestWrapper2000, fuelTask, resampling = kFold)

parallelStop()