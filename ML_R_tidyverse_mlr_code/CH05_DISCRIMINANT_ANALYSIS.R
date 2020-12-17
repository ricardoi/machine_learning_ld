###############################################
#          SOURCE CODE FOR CHAPTER 5          #
###############################################

# LOAD PACKAGES ----
library(mlr)

library(tidyverse)

# LOAD DATA ----
install.packages("HDclassif")

data(wine, package = "HDclassif")

wineTib <- as_tibble(wine)

wineTib

# CLEAN DATA ----
names(wineTib) <- c("Class", "Alco", "Malic", "Ash", "Alk", "Mag", 
                    "Phe", "Flav", "Non_flav", "Proan", "Col", "Hue", 
                    "OD", "Prol")

wineTib$Class <- as.factor(wineTib$Class)

wineTib

# PLOTTING THE DATA ----
wineUntidy <- gather(wineTib, "Variable", "Value", -Class)

ggplot(wineUntidy, aes(Class, Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  geom_boxplot() +
  theme_bw()

# CREATE TASK AND LEARNER, AND TRAIN MODEL ----
wineTask <- makeClassifTask(data = wineTib, target = "Class")

lda <- makeLearner("classif.lda")

ldaModel <- train(lda, wineTask)

# EXTRACTING DISCRIMINANT FUNCTION SCORES FOR EACH CASE ----
ldaModelData <- getLearnerModel(ldaModel)

ldaPreds <- predict(ldaModelData)$x

head(ldaPreds)

# PLOT DISCRIMINANT FUNCTIONS ----
wineTib %>%
  mutate(LD1 = ldaPreds[, 1], 
         LD2 = ldaPreds[, 2]) %>%
  ggplot(aes(LD1, LD2, col = Class)) +
  geom_point() +
  stat_ellipse() +
  theme_bw()

# MAKE QDA LEARNER AND TRAIN MODEL----
qda <- makeLearner("classif.qda")

qdaModel <- train(qda, wineTask)

# CROSS-VALIDATING MODELS ----
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

ldaCV <- resample(learner = lda, task = wineTask, resampling = kFold,
                    measures = list(mmce, acc))

qdaCV <- resample(learner = qda, task = wineTask, resampling = kFold,
                  measures = list(mmce, acc))
ldaCV$aggr

qdaCV$aggr

# CALCULATE CONFUSION MATRICES ----
calculateConfusionMatrix(ldaCV$pred, relative = TRUE)

calculateConfusionMatrix(qdaCV$pred, relative = TRUE)

# USING THE QDA MODEL TO MAKE PREDICTIONS ----
poisoned <- tibble(Alco = 13, Malic = 2, Ash = 2.2, Alk = 19, Mag = 100, 
                   Phe = 2.3, Flav = 2.5, Non_flav = 0.35, Proan = 1.7,
                   Col = 4, Hue = 1.1, OD = 3, Prol = 750)

predict(qdaModel, newdata = poisoned)

# SOLUTIONS TO EXERCISES ----
# 2
wineDiscr <- wineTib %>%
  mutate(LD1 = ldaPreds[, 1], LD2 = ldaPreds[, 2]) %>%
  select(Class, LD1, LD2)

wineDiscrTask <- makeClassifTask(data = wineDiscr, target = "Class")

knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))

gridSearch <- makeTuneControlGrid()

cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)

tunedK <- tuneParams("classif.knn", task = wineDiscrTask, 
                     resampling = cvForTuning, 
                     par.set = knnParamSpace, 
                     control = gridSearch)

tunedK$x

tunedKnn <- setHyperPars(makeLearner("classif.knn"), par.vals = tunedK$x)

tunedKnnModel <- train(tunedKnn, wineDiscrTask)