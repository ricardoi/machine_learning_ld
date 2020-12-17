###############################################
#          SOURCE CODE FOR CHAPTER 3          #
###############################################

# INSTALLING AND LOADING PACKAGES ----
install.packages("mlr", dependencies = TRUE) # could take several minutes
# only needed once on any R installation

library(mlr)

library(tidyverse)

# LOADING DIABETES DATA ----
data(diabetes, package = "mclust")

diabetesTib <- as_tibble(diabetes)

summary(diabetesTib)

diabetesTib

# PLOT THE RELATIONSHIPS IN THE DATA ----
ggplot(diabetesTib, aes(glucose, insulin, col = class)) + 
  geom_point()  +
  theme_bw()

ggplot(diabetesTib, aes(sspg, insulin, col = class)) + 
  geom_point() +
  theme_bw()

ggplot(diabetesTib, aes(sspg, glucose, col = class)) + 
  geom_point() +
  theme_bw()

# DEFINING THE DIABETES TASK ----
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")

diabetesTask

# DEFINING THE KNN LEARNER ----
knn <- makeLearner("classif.knn", par.vals = list("k" = 2))

# LISTING ALL OF MLR'S LEARNERS ----
listLearners()$class

# or list them by function:
listLearners("classif")$class

listLearners("regr")$class

listLearners("cluster")$class

# DEFINE MODEL ----
knnModel <- train(knn, diabetesTask)

# TESTING PERFORMANCE ON TRAINING DATA (VERY BAD PRACTICE) ----
knnPred <- predict(knnModel, newdata = diabetesTib)

performance(knnPred, measures = list(mmce, acc))

# PERFORMING HOLD-OUT CROSS-VALIDATION ----
holdout <- makeResampleDesc(method = "Holdout", split = 2/3, 
                            stratify = TRUE)

holdoutCV <- resample(learner = knn, task = diabetesTask, 
                      resampling = holdout,
                      measures = list(mmce, acc))

holdoutCV$aggr

calculateConfusionMatrix(holdoutCV$pred, relative = TRUE)

# PERFORMING REPEATED K-FOLD CROSS-VALIDATION ----
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

kFoldCV <- resample(learner = knn, task = diabetesTask, 
                    resampling = kFold, measures = list(mmce, acc))

kFoldCV$aggr

kFoldCV$measures.test

calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

# PERFORMING LEAVE-ONE-OUT CROSS-VALIDATION ----
LOO <- makeResampleDesc(method = "LOO")

LOOCV <- resample(learner = knn, task = diabetesTask, resampling = LOO,
                  measures = list(mmce, acc))

LOOCV$aggr

calculateConfusionMatrix(LOOCV$pred, relative = TRUE)

# HYPERPARAMETER TUNING OF K ----
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))

gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = diabetesTask, 
                     resampling = cvForTuning, 
                     par.set = knnParamSpace, 
                     control = gridSearch)

tunedK

tunedK$x

knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean",
                    plot.type = "line") +
                    theme_bw()

# TRAINING FINAL MODEL WITH TUNED K ----
tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, diabetesTask)

# INCLUDING HYPERPARAMETER TUNING INSIDE NESTED CROSS-VALIDATION ----
inner <- makeResampleDesc("CV")

outer <- makeResampleDesc("RepCV", folds = 10, reps = 5)

knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner, 
                              par.set = knnParamSpace, 
                              control = gridSearch) 

cvWithTuning <- resample(knnWrapper, diabetesTask, resampling = outer)

cvWithTuning

# USING THE MODEL TO MAKE PREDICTIONS ----
newDiabetesPatients <- tibble(glucose = c(82, 108, 300), 
                              insulin = c(361, 288, 1052),
                              sspg = c(200, 186, 135))

newDiabetesPatients

newPatientsPred <- predict(tunedKnnModel, newdata = newDiabetesPatients)

getPredictionResponse(newPatientsPred)

# EXERCISES ----
# 1
ggplot(diabetesTib, aes(glucose, insulin, 
                        shape = class)) + 
  geom_point()  +
  theme_bw()

ggplot(diabetesTib, aes(glucose, insulin, 
                        shape = class, col = class)) + 
  geom_point()  +
  theme_bw()

# 2
holdoutNoStrat <- makeResampleDesc(method = "Holdout", split = 0.9, 
                            stratify = FALSE)

# 3
kFold500 <- makeResampleDesc(method = "RepCV", folds = 3, reps = 500, 
                          stratify = TRUE)

kFoldCV500 <- resample(learner = knn, task = diabetesTask, 
                    resampling = kFold500, measures = list(mmce, acc))

kFold5 <- makeResampleDesc(method = "RepCV", folds = 3, reps = 5, 
                             stratify = TRUE)

kFoldCV5 <- resample(learner = knn, task = diabetesTask, 
                       resampling = kFold5, measures = list(mmce, acc))

kFoldCV500$aggr
kFoldCV5$aggr

calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

# 4
makeResampleDesc(method = "LOO", stratify = TRUE)

makeResampleDesc(method = "LOO", reps = 5)

# both will result in an error as LOO cross-validation cannot
# be stratified or repeated

# 5
data(iris)

irisTask <- makeClassifTask(data = iris, target = "Species")

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:25))

gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = irisTask, 
                     resampling = cvForTuning, 
                     par.set = knnParamSpace, 
                     control = gridSearch)

tunedK

tunedK$x

knnTuningData <- generateHyperParsEffectData(tunedK)

plotHyperParsEffect(knnTuningData, x = "k", y = "mmce.test.mean",
                    plot.type = "line") +
                    theme_bw()

tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, irisTask)

# 6
inner <- makeResampleDesc("CV")

outerHoldout <- makeResampleDesc("Holdout", split = 2/3, stratify = TRUE)

knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner, 
                              par.set = knnParamSpace, 
                              control = gridSearch) 

holdoutCVWithTuning <- resample(knnWrapper, irisTask, 
                                resampling = outerHoldout)

holdoutCVWithTuning

# 7
outerKfold <- makeResampleDesc("CV", iters = 5, stratify = TRUE)

kFoldCVWithTuning <- resample(knnWrapper, irisTask, 
                              resampling = outerKfold)

kFoldCVWithTuning

resample(knnWrapper, irisTask, resampling = outerKfold)

# repeat each validation procedure 10 times and save the mmce value 
# WARNING: this may take a few minutes to complete

kSamples <- map_dbl(1:10, ~resample(
  knnWrapper, irisTask, resampling = outerKfold)$aggr
)

hSamples <- map_dbl(1:10, ~resample(
  knnWrapper, irisTask, resampling = outerHoldout)$aggr
)

hist(kSamples, xlim = c(0, 0.11))
hist(hSamples, xlim = c(0, 0.11))

# holdout CV gives more variable estimates of model performance 