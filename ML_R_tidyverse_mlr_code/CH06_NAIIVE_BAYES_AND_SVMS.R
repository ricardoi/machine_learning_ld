###############################################
#          SOURCE CODE FOR CHAPTER 6          #
###############################################

# NAIIVE BAYES # ----
# LOAD PACKAGES ----
library(mlr)

library(tidyverse)

# LOAD DATA ----
data(HouseVotes84, package = "mlbench")

votesTib <- as_tibble(HouseVotes84)

votesTib

?mlbench::HouseVotes84

map_dbl(votesTib, ~ sum(is.na(.)))

# PLOTTING THE DATA ----
votesUntidy <- gather(votesTib, "Variable", "Value", -Class)

ggplot(votesUntidy, aes(Class, fill = Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_bar(position = "fill") +
  theme_bw()
#ggsave("CH06_VOTES_MLR.pdf", width = 10, height = 6)

# MAKE TASK AND LEARNER, AND TRAIN MODEL ----
votesTask <- makeClassifTask(data = votesTib, target = "Class")

bayes <- makeLearner("classif.naiveBayes")

bayesModel <- train(bayes, votesTask)

# CROSS-VALIDATING THE MODEL ----
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

bayesCV <- resample(learner = bayes, task = votesTask, 
                    resampling = kFold,
                    measures = list(mmce, acc, fpr, fnr))

bayesCV$aggr

# USING THE MODEL TO MAKE 
politician <- tibble(V1 = "n", V2 = "n", V3 = "y", V4 = "n", V5 = "n", 
                     V6 = "y", V7 = "y", V8 = "y", V9 = "y", V10 = "y", 
                     V11 = "n", V12 = "y", V13 = "n", V14 = "n", V15 = "y", 
                     V16 = "n")

politicianPred <- predict(bayesModel, newdata = politician)

getPredictionResponse(politicianPred)

getLearnerModel(bayesModel)

# SUPPORT VECTOR MACHINE # ----
# LOAD DATA ----
data(spam, package = "kernlab")

spamTib <- as_tibble(spam)

spamTib

# CREATE TASK AND LEARNER ----
spamTask <- makeClassifTask(data = spamTib, target = "type")

svm <- makeLearner("classif.svm")

# TUNE HYPERPARAMETERS ----
getParamSet("classif.svm")

kernels <- c("polynomial", "radial", "sigmoid")

svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = kernels),
  makeIntegerParam("degree", lower = 1, upper = 3),
  makeNumericParam("cost", lower = 0.1, upper = 10),
  makeNumericParam("gamma", lower = 0.1, 10))

randSearch <- makeTuneControlRandom(maxit = 20)

cvForTuning <- makeResampleDesc("Holdout", split = 2/3)

library(parallel)
library(parallelMap)

detectCores()

parallelStartSocket(cpus = detectCores())

tunedSvmPars <- tuneParams("classif.svm", task = spamTask, # ~43 sec
                     resampling = cvForTuning, 
                     par.set = svmParamSpace, 
                     control = randSearch)

parallelStop()

tunedSvmPars

tunedSvmPars$x

# TRAINING FINAL MODEL WITH TUNED HYPERPARAMETERS ----
tunedSvm <- setHyperPars(svm, par.vals = tunedSvmPars$x)

tunedSvmModel <- train(tunedSvm, spamTask)

# INCLUDING HYPERPARAMETER TUNING INSIDE NESTED CROSS-VALIDATION ----
outer <- makeResampleDesc("CV", iters = 3)

svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning, 
                              par.set = svmParamSpace, 
                              control = randSearch) 

parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(svmWrapper, spamTask, resampling = outer) # ~1 min

parallelStop()

cvWithTuning

# EXERCISES ----
# 1
map_dbl(votesTib, ~ length(which(. == "y")))

# 2
getLearnerModel(bayesModel)

# the prior probabilities are 0.61 for democrat and 0.39 for republican 
# (at the time these data were collected!)

# the likelihoods are shown in 2x2 tables for each vote

# 3
votesTib[] <- map(votesTib, as.character)
votesTib[is.na(votesTib)] <- "a"
votesTib[] <- map(votesTib, as.factor)

votesTask <- makeClassifTask(data = votesTib, target = "Class")

bayes <- makeLearner("classif.naiveBayes")

kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

bayesCV <- resample(learner = bayes, task = votesTask, resampling = kFold,
                    measures = list(mmce, acc, fpr, fnr))

bayesCV$aggr

# only a very slight increase in accuracy

# 4
svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", values = "linear"),
  makeNumericParam("cost", lower = 0.1, upper = 100))

randSearch <- makeTuneControlRandom(maxit = 100)

cvForTuning <- makeResampleDesc("Holdout", split = 2/3)

outer <- makeResampleDesc("CV", iters = 3)

svmWrapper <- makeTuneWrapper("classif.svm", resampling = cvForTuning, 
                              par.set = svmParamSpace, 
                              control = randSearch) 

parallelStartSocket(cpus = detectCores())

cvWithTuning <- resample(svmWrapper, spamTask, resampling = outer) # ~1 min

parallelStop()

cvWithTuning
