###############################################
#          SOURCE CODE FOR CHAPTER 11          #
###############################################

# LOAD PACKAGES ----
library(mlr)

library(tidyverse)

# LOAD DATA ----
#install.packages("lasso2")

data(Iowa, package = "lasso2")

iowaTib <- as_tibble(Iowa)

iowaTib

# PLOTTING THE DATA ----
iowaUntidy <- gather(iowaTib, "Variable", "Value", -Yield)

ggplot(iowaUntidy, aes(Value, Yield)) +
  facet_wrap(~ Variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
#ggsave("Iowa.pdf", width = 10, height = 5)

# MAKE TASK AND RIDGE LEARNER ----
iowaTask <- makeRegrTask(data = iowaTib, target = "Yield")

ridge <- makeLearner("regr.glmnet", alpha = 0, id = "ridge")

# FEATURE SELECTION FILTER METHOD ----
filterVals <- generateFilterValuesData(iowaTask)

plotFilterValues(filterVals) + theme_bw()
#ggsave("Filtervals.pdf", width = 10, height = 5)

# TUNING LAMBDA FOR RIDGE ----
# https://stackoverflow.com/questions/50995525/nested-resampling-lasso-regr-cvglment-using-mlr

ridgeParamSpace <- makeParamSet(
  makeNumericParam("s", lower = 0, upper = 15))

randSearch <- makeTuneControlRandom(maxit = 200)

cvForTuning <- makeResampleDesc("RepCV", folds = 3, reps = 10)

library(parallel)
library(parallelMap)

parallelStartSocket(cpus = detectCores())

tunedRidgePars <- tuneParams(ridge, task = iowaTask, # ~30 sec
                            resampling = cvForTuning, 
                            par.set = ridgeParamSpace, 
                            control = randSearch)

parallelStop()

tunedRidgePars

# PLOTTING THE RANDOM SEARCH ----
ridgeTuningData <- generateHyperParsEffectData(tunedRidgePars)

plotHyperParsEffect(ridgeTuningData, x = "s", y = "mse.test.mean",
                    plot.type = "line") +
  theme_bw()

#ggsave("Ridge_lambda.pdf", width = 10, height = 5)

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedRidge <- setHyperPars(ridge, par.vals = tunedRidgePars$x)

tunedRidgeModel <- train(tunedRidge, iowaTask)

# INTERPRETTING THE RIDGE REGRESSION MODEL ----
ridgeModelData <- getLearnerModel(tunedRidgeModel)

# plot(ridgeModelData, xvar = "lambda", label = TRUE)
# plot(ridgeModelData, xvar = "norm", label = TRUE)
ridgeCoefs <- coef(ridgeModelData, s = tunedRidgePars$x$s)

ridgeCoefs

lmCoefs <- coef(lm(Yield ~ ., data = iowaTib))

lmCoefs

coefTib <- tibble(Coef = rownames(ridgeCoefs)[-1],
                       Ridge = as.vector(ridgeCoefs)[-1],
                       Lm = as.vector(lmCoefs)[-1])

coefUntidy <- gather(coefTib, key = Model, value = Beta, -Coef)

ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) +
  geom_bar(stat = "identity", col = "black") +
  facet_wrap(~Model) +
  theme_bw()  + 
  theme(legend.position = "none")

#ggsave("Ridge_coefs.pdf", width = 10, height = 5)

# MAKE LASSO LEARNER ----
lasso <- makeLearner("regr.glmnet", alpha = 1, id = "lasso")

# TUNING LAMBDA FOR LASSO ----
lassoParamSpace <- makeParamSet(
  makeNumericParam("s", lower = 0, upper = 15))

parallelStartSocket(cpus = detectCores())

tunedLassoPars <- tuneParams(lasso, task = iowaTask, # ~30 sec
                             resampling = cvForTuning, 
                             par.set = lassoParamSpace, 
                             control = randSearch)

parallelStop()

tunedLassoPars

# PLOTTING THE RANDOM SEARCH ----
lassoTuningData <- generateHyperParsEffectData(tunedLassoPars)

plotHyperParsEffect(lassoTuningData, x = "s", y = "mse.test.mean",
                    plot.type = "line") +
  theme_bw()

#ggsave("Lasso_lambda.pdf", width = 10, height = 5)

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedLasso <- setHyperPars(lasso, par.vals = tunedLassoPars$x)

tunedLassoModel <- train(tunedLasso, iowaTask)

# INTERPRETTING THE LASSO REGRESSION MODEL ----
lassoModelData <- getLearnerModel(tunedLassoModel)

# plot(lassoModelData, xvar = "lambda", label = TRUE)
# plot(lassoModelData, xvar = "norm", label = TRUE)
lassoCoefs <- coef(lassoModelData, s = tunedLassoPars$x$s)
#lassoCoefs <- coef(lassoModelData, s = 1.37) #to get the same s as me

lassoCoefs

coefTib$LASSO <- as.vector(lassoCoefs)[-1]

coefUntidy <- gather(coefTib, key = Model, value = Beta, -Coef)

ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) +
  geom_bar(stat = "identity", col = "black") +
  facet_wrap(~ Model) +
  theme_bw() + 
  theme(legend.position = "none")

#ggsave("LASSO_coefs.pdf", width = 10, height = 5)

# MAKE ELASTIC NET LEARNER ----
elastic <- makeLearner("regr.glmnet", id = "elastic")

# TUNING LAMBDA AND ALPHA FOR ELASTIC NET ----
elasticParamSpace <- makeParamSet(
  makeNumericParam("s", lower = 0, upper = 10),
  makeNumericParam("alpha", lower = 0, upper = 1))

randSearchElastic <- makeTuneControlRandom(maxit = 400)

parallelStartSocket(cpus = detectCores())

tunedElasticPars <- tuneParams(elastic, task = iowaTask, # ~1 min
                             resampling = cvForTuning, 
                             par.set = elasticParamSpace, 
                             control = randSearchElastic)

parallelStop()

tunedElasticPars

elasticTuningData <- generateHyperParsEffectData(tunedElasticPars)

plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha", 
                    z = "mse.test.mean", interpolate = "regr.kknn", 
                    plot.type = "heatmap") +
  scale_fill_gradientn(colours = terrain.colors(5)) +
  geom_point(x = tunedElasticPars$x$s, y = tunedElasticPars$x$alpha) + 
  theme_bw()

#ggsave("Elastic_tuning.pdf", width = 10, height = 5)

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedElastic <- setHyperPars(elastic, par.vals = tunedElasticPars$x)

tunedElasticModel <- train(tunedElastic, iowaTask)

# INTERPRETTING THE LASSO REGRESSION MODEL ----
elasticModelData <- getLearnerModel(tunedElasticModel)

# plot(elasticModelData, xvar = "lambda", label = TRUE)
# plot(elasticModelData, xvar = "norm", label = TRUE)
elasticCoefs <- coef(elasticModelData, s = tunedElasticPars$x$s)

coefTib$Elastic <- as.vector(elasticCoefs)[-1]

coefUntidy <- gather(coefTib, key = Model, value = Beta, -Coef)

ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  facet_wrap(~ Model) +
  theme_bw()

#ggsave("Elastic_coefs.pdf", width = 10, height = 5)

# BENCHMARKING EACH MODEL BUILDING PROCESS ----
# MAKE TUNING WRAPPERS
ridgeWrapper <- makeTuneWrapper(ridge, resampling = cvForTuning,
                                par.set = ridgeParamSpace, 
                                control = randSearch) 

lassoWrapper <- makeTuneWrapper(lasso, resampling = cvForTuning,
                                par.set = lassoParamSpace, 
                                control = randSearch) 

elasticWrapper <- makeTuneWrapper(elastic, resampling = cvForTuning,
                                  par.set = elasticParamSpace, 
                                  control = randSearchElastic) 

learners = list(ridgeWrapper, lassoWrapper, elasticWrapper, "regr.lm")


library(parallel)
library(parallelMap)

kFold3 <- makeResampleDesc("CV", iters = 3)

parallelStartSocket(cpus = detectCores())

bench <- benchmark(learners, iowaTask, kFold3)

parallelStop()

bench



# SOLUTIONS TO EXERCISES ----
# 1
ridgeParamSpaceExtended <- makeParamSet(
  makeNumericParam("s", lower = 0, upper = 50))

parallelStartSocket(cpus = detectCores())

tunedRidgeParsExtended <- tuneParams(ridge, task = iowaTask, # ~30 sec
                             resampling = cvForTuning, 
                             par.set = ridgeParamSpaceExtended, 
                             control = randSearch)

parallelStop()

ridgeTuningDataExtended <- generateHyperParsEffectData(
                                      tunedRidgeParsExtended)

plotHyperParsEffect(ridgeTuningDataExtended, x = "s", y = "mse.test.mean",
                    plot.type = "line") +
  theme_bw()

# the previous value of s was not just a local minimum, 
# but the global minimum

# 2
coefTibInts <- tibble(Coef = rownames(ridgeCoefs),
                  Ridge = as.vector(ridgeCoefs),
                  Lm = as.vector(lmCoefs))

coefUntidyInts <- gather(coefTibInts, key = Model, value = Beta, -Coef)

ggplot(coefUntidyInts, aes(reorder(Coef, Beta), Beta, fill = Model)) +
  geom_bar(stat = "identity", col = "black") +
  facet_wrap(~Model) +
  theme_bw()  + 
  theme(legend.position = "none")

# the intercepts are different. The intercept isn't included when
# calculating the L2 norm, but is the value of the outcome when all
# the predictors are zero. As ridge regression changes the parameter
# estimates of the predictors, the intercept changes as a result

# 3
plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha", 
                    z = "mse.test.mean", interpolate = "regr.kknn", 
                    plot.type = "contour", show.experiments = TRUE) +
  scale_fill_gradientn(colours = terrain.colors(5)) +
  geom_point(x = tunedElasticPars$x$s, y = tunedElasticPars$x$alpha) + 
  theme_bw()

plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha", 
                    z = "mse.test.mean", plot.type = "scatter") +
  theme_bw()

# 4
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  theme_bw()

# 5
yieldOnly <- select(iowaTib, Yield)

yieldOnlyTask <- makeRegrTask(data = yieldOnly, target = "Yield")

lassoStrict <- makeLearner("regr.glmnet", lambda = 500)

loo <- makeResampleDesc("LOO")

resample("regr.lm", yieldOnlyTask, loo)

resample(lassoStrict, iowaTask, loo)

# the MSE values are identical. This is because when lambda is high
# enough, all predictors will be removed from the model, just as if
# we trained a model with no predictors

# 6
install.packages("plotmo")

library(plotmo)

plotres(elasticModelData)

plotres(ridgeModelData)

plotres(lassoModelData)

# the first plot shows the estimated slope for each parameter for 
# different values of (log) lambda. Notice the different shape 
# between ridge and LASSO
