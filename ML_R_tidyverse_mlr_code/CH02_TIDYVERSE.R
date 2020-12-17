###############################################
#          SOURCE CODE FOR CHAPTER 2          #
###############################################

# INSTALLING AND LOADING THE TIDYVERSE ----
# install and load together (recommended)
install.packages("tidyverse") # only needed once on any R installation
library(tidyverse)

# or install and load individually as needed
# install.packages(c("tibble", "dplyr", "ggplot2", "tidyr"))
# library(tibble)
# library(dplyr)
# library(ggplot2)
# library(tidyr)

# CREATING TIBBLES WITH tibble() ----
myTib <- tibble(x =  1:4, 
                y = c("london", "beijing", "las vegas", "berlin"))

myTib

# CONVERTING DATA FRAMES TO TIBBLES WITH as_tibble() ----
myDf <- data.frame(x =  1:4, 
                   y = c("london", "beijing", "las vegas", "berlin"))

dfToTib <- as_tibble(myDf)

dfToTib

# TIBBLES DON'T CONVERT STRINGS TO FACTORS BY DEFAULT ----
myDf <- data.frame(x =  1:4, 
                   y = c("london", "beijing", "las vegas", "berlin"))

myDfNotFactor <- data.frame(x =  1:4, 
                            y = c("london", "beijing", "las vegas", "berlin"),
                            stringsAsFactors = FALSE)

myTib <- tibble(x =  1:4, 
                y = c("london", "beijing", "las vegas", "berlin"))

class(myDf$y)

class(myDfNotFactor$y)

class(myTib$y)

# IF YOU WANT TO CREATE A FACTOR, WRAP THE c() FUNCTION INSIDE factor() ----
myTib <- tibble(x =  1:4, 
                y = factor(c("london", "beijing", "las vegas", "berlin")))
myTib

# PRINTING A TIBBLE KEEPS THE OUTPUT CONCISE ----
data(starwars)

starwars

as.data.frame(starwars)

# SUBSETTING WITH [ ALWAYS RETURNS ANOTHER TIBBLE ----
myDf[, 1]

myTib[, 1]

myTib[[1]]

myTib$x

# VARIABLE CREATION IN tibble() IS SEQUENTIAL ----
sequentialTib <- tibble(nItems = c(12, 45, 107),
                        cost = c(0.5, 1.2, 1.8),
                        totalWorth = nItems * cost)

sequentialTib

# EXPLORING THE CO2 DATASET ----
data(CO2)

CO2tib <- as_tibble(CO2)

CO2tib

# SELECTING COLUMNS WITH select() ----
selectedData <- select(CO2tib, 1, 2, 3, 5)

selectedData

# FILTERING DATA WITH filter() ----
filteredData <- filter(selectedData, uptake > 16)

filteredData

# GROUPING DATA WITH group_by() ----
groupedData <- group_by(filteredData, Plant)

groupedData

# SUMMARIZING DATA WITH summarize() ----
summarizedData <- summarize(groupedData, meanUp = mean(uptake), 
                            sdUp = sd(uptake))

summarizedData

# CREATING NEW VARIABLES WITH mutate() ----
mutatedData <- mutate(summarizedData,  CV = (sdUp / meanUp) * 100)

mutatedData

# ARRANGING DATA WITH arrange() ----
arrangedData <- arrange(mutatedData, CV)

arrangedData

# USING THE %>% ("PIPE") OPERATOR ----
c(1, 4, 7, 3, 5) %>% mean()

# COMBINING DPLYR VERBS WITH THE %>% OPERATOR ----
arrangedData <- CO2tib %>%
  select(c(1:3, 5)) %>%
  filter(uptake > 16) %>%
  group_by(Plant) %>%
  summarize(meanUp = mean(uptake), sdUp = sd(uptake)) %>%
  mutate(CV = (sdUp / meanUp) * 100) %>%
  arrange(CV)

arrangedData

# PLOTTING THE IRIS DATASET WITH ggplot() ----
data(iris)
myPlot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme_bw()

myPlot

# ADDING ADDITIONAL GEOMETRIC OBJECTS ("GEOMS") AS PLOT LAYERS ----
myPlot +
  geom_density_2d() +
  geom_smooth()

# MAPPING SPECIES TO THE SHAPE AND COLOR AESTHETICS ----
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) +
  geom_point()  +
  theme_bw()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()  +
  theme_bw()

# FACETING BY SPECIES ----
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  facet_wrap(~ Species) +
  geom_point()  +
  theme_bw()

# CREATING AN UNTIDY TIBBLE ----
patientData <- tibble(Patient = c("A", "B", "C"),
                      Month0 = c(21, 17, 29),
                      Month3 = c(20, 21, 27),
                      Month6 = c(21, 22, 23))

patientData

# CONVERTING UNTIDY DATA TO TIDY FORMA USING gather() ----
tidyPatientData <- gather(patientData, key = Month, 
                          value = BMI, -Patient)
tidyPatientData

# or the same can be achieved with:
gather(patientData, key = Month, value = BMI, Month0:Month6)
# or:
gather(patientData, key = Month, value = BMI, c(Month0, Month3, Month6))

# CONVERTING DATA INTO WIDE FORMAT WITH spread() ----
spread(tidyPatientData, key = Month, value = BMI)

# EXAMPLE OF PURE FUNCTION VS ONE WITH SIDE EFFECTS ----
a <- 20

pure <- function() {
  a <- a + 1
  a
}

side_effect <- function() {
  a <<- a + 1
  a
}

c(pure(), pure())

c(side_effect(), side_effect())

# USING purrr FUNCTIONS FOR VECTORIZATION ----
listOfNumerics <- list(a = rnorm(5), 
                       b = rnorm(9),
                       c = rnorm(10))

listOfNumerics

elementLengths <- vector("list", length = 3)

for(i in seq_along(listOfNumerics)) {
  elementLengths[[i]] <- length(listOfNumerics[[i]])
}

elementLengths

map(listOfNumerics, length)

map_int(listOfNumerics, length)

map_chr(listOfNumerics, length)

map_lgl(listOfNumerics, length)

map_df(listOfNumerics, length)

map(listOfNumerics, ~. + 2)

par(mfrow = c(1, 3))

walk(listOfNumerics, hist)

iwalk(listOfNumerics, ~hist(.x, main = .y))

multipliers <- list(0.5, 10, 3)

map2(.x = listOfNumerics, .y = multipliers, ~.x * .y)

arguments <- expand.grid(n = c(100, 200),
                         mean = c(1, 10),
                         sd = c(1, 10))

arguments

par(mfrow = c(2, 4))

pmap(arguments, rnorm) %>%
  iwalk(~hist(.x, main = paste("Element", .y)))

# SOLUTIONS TO EXERCISES ----
# 1
library(tidyverse)

data(mtcars)

mtcarsTib <- as_tibble(mtcars)

summary(mtcarsTib)

# 2
select(mtcarsTib, c(-qsec, -vs))
# or
select(mtcarsTib, c(-7, -8))

# 3
filter(mtcarsTib, cyl != 8)

# 4
mtcarsTib %>%
  group_by(gear) %>%
  summarize(mpgMed = median(mpg), dispMed = median(disp)) %>%
  mutate(mpgOverDisp = mpgMed / dispMed)

# 5
ggplot(mtcarsTib, aes(drat, wt, col = carb)) +
  geom_point()

ggplot(mtcarsTib, aes(drat, wt, col = as.factor(carb))) +
  geom_point()

# 6
gather(mtcarsTib, key = "variable", value = "value", c(vs, am, gear, carb))
# or
gather(mtcarsTib, key = "variable", value = "value", c(8:11))

# 7
map_lgl(mtcars, ~sum(.) > 1000)
# or
map_lgl(mtcars, function(.) sum(.) > 1000)
