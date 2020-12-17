###############################################
#          SOURCE CODE FOR CHAPTER 10         #
###############################################

# LOAD PACKAGES ----
library(mlr)

library(tidyverse)

# LOAD DATA ----
data(Ozone, package = "mlbench")

ozoneTib <- as_tibble(Ozone)

names(ozoneTib) <- c("Month", "Date", "Day", "Ozone", "Press_height", 
                     "Wind", "Humid", "Temp_Sand", "Temp_Monte", 
                     "Inv_height", "Press_grad", "Inv_temp", "Visib")

ozoneTib

ozoneClean <- mutate_all(ozoneTib, as.numeric) %>%
  filter(is.na(Ozone) == FALSE)

ozoneClean

# DEFINING GAM LEARNER AND TASK ----
ozoneForGam <- mutate(ozoneClean,
                      DayOfYear = as.numeric(interaction(Date, Month))) %>%
  select(c(-"Date", -"Month"))

ggplot(ozoneForGam, aes(DayOfYear, Ozone)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#ggsave("DayOfYear.pdf", width = 10, height = 6)

gamTask <- makeRegrTask(data = ozoneForGam, target = "Ozone")

# DEFINE IMPUTATION AND FEATURE SELECTION WRAPPERS ----
imputeMethod <- imputeLearner("regr.rpart")

gamImputeWrapper <- makeImputeWrapper("regr.gamboost",
                                      classes = list(numeric = imputeMethod))

gamFeatSelControl <- makeFeatSelControlSequential(method = "sfbs")

kFold <- makeResampleDesc("CV", iters = 10)

gamFeatSelWrapper <- makeFeatSelWrapper(learner = gamImputeWrapper,
                                        resampling = kFold,
                                        control = gamFeatSelControl)

# CROSS-VALIDATE GAM MODEL-BUILDING PROCESS ----
holdout <- makeResampleDesc("Holdout")

gamCV <- resample(gamFeatSelWrapper, gamTask, resampling = holdout)

gamCV

# EXPLORING GAM MODELS ----
library(parallel)

library(parallelMap)

parallelStartSocket(cpus = detectCores())

gamModel <- train(gamFeatSelWrapper, gamTask)

parallelStop()

gamModelData <- getLearnerModel(gamModel, more.unwrap = TRUE)

summary(gamModelData)

par(mfrow = c(3, 3))
plot(gamModelData, type = "l")
plot(gamModelData$predict(), resid(gamModelData))
qqnorm(resid(gamModelData))
qqline(resid(gamModelData))
par(mfrow = c(1, 1))

# EXERCISES ----
# 1
interaction(1:4, c("a", "b", "c", "d"))

# 2
ggplot(ozoneForGam, aes(DayOfYear, Ozone)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", col = "red") +
  theme_bw()

# the quadratic polynomial does a pretty good job of modeling the
# relationship between the variables

# 3
filterWrapperImp <- makeFilterWrapper(learner = gamImputeWrapper, 
                                   fw.method = "linear.correlation")

filterParam <- makeParamSet(
  makeIntegerParam("fw.abs", lower = 1, upper = 12)
)

gridSearch <- makeTuneControlGrid()

tuneWrapper <- makeTuneWrapper(learner = filterWrapperImp, 
                               resampling = kFold,
                               par.set = filterParam, 
                               control = gridSearch)

filterGamCV <- resample(tuneWrapper, gamTask, resampling = holdout)

filterGamCV

# we have a similar MSE estimate for the filter method, but it is
# considerably faster than the wrapper method