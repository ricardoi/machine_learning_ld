#'@title: kk-neirest neighbor
#'@concept: Machine Learning book discussion
#
#
#

# install.packages('mlr')
library(mclust)
library(tidyverse)
library(mlr)

#loading data
data(diabetes)

# Exploring data
diabetes.t <- as.tibble(diabetes)
summary(diabetes.t)

ggplot(diabetes.t, aes(x=glucose, y= insulin, col=class))+
  geom_point()+
  theme_bw()

ggplot(diabetes.t, aes(x=sspg, y= insulin, col=class))+
  geom_point()+
  theme_bw()

ggplot(diabetes.t, aes(x=sspg, y= glucose, col=class))+
  geom_point()+
  theme_bw()

# Building classifier
 diabetesTask <- makeClassifTask(data= diabetes.t, target= "class")
 diabetesTask
 
# Creating a learner
 knn <- makeLearner("classif.knn", par.vals = list("k" = 2))

 # Training 
 knnModel <- train(knn, diabetesTask)

 # New iteration
 knnPred <- predict(knnModel, newdata = diabetes.t)
# meusrument args
 # mmce = mean misclassification error
 # acc = accuracy
 performance(knnPred, measures = list(mmce, acc)) 
 
# Cross Validation (CV)
 diabetesTask <- makeClassifTask(data= diabetes.t, target = "class")
 knn <- makeLearner("classif.knn", par.vals = list("k" = 2))
 
 holdout <-  makeResampleDesc(method= "Holdout", split = 2/3, 
                                stratify= T)
# Resampling
 d.holdoutCV <- mlr::resample(task = diabetesTask, learner = knn, 
                         resampling = holdout, measures= list(mmce, acc))

# Calculating a confusion matrix
 calculateConfusionMatrix(d.holdoutCV$pred, relative = T) 
  
# k-fold CV
 # Create resample model RepCV
 kfold <-  makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                           stratify = T)
 kfoldCV <-  mlr::resample(task = diabetesTask, learner = knn, 
                         resampling = kfold, measures = list(mmce, acc))
# Average performance
 kfoldCV$aggr

# Calculating a confusion matrix CV
 calculateConfusionMatrix(kfoldCV$pred, relative = T)


# Cross Validation (CV) 
 # Create resample model - Leave One Out (LOO)
 LOO <- makeResampleDesc(method = "LOO")

 LOOCV <- mlr::resample(task = diabetesTask, learner = knn, 
                       resampling = LOO, measures = list(mmce, acc))
# Average performance
 LOOCV$aggr
