###############################################
#          SOURCE CODE FOR CHAPTER 14         #
###############################################

# LOADING TIDYVERSE ----
library(tidyverse)

# LOADING DATA ----
data(banknote, package = "mclust")

swissTib <- as_tibble(banknote)

swissTib

# t-SNE ----
#install.packages("Rtsne")

library(Rtsne)

# CREATE GRID OF t-SNE HYPERPARAMETERS ----
tsneHyperPars <- expand.grid(perplexity = c(1, 10, 30, 40, 50), 
                             theta      = seq(0.0, 1.0, 0.25),
                             eta        = c(1, 100, 200, 300, 400),
                             max_iter   = c(100, 300, 500, 700, 1000))

tsne <- pmap(tsneHyperPars, Rtsne, X = swissTib[, -1], verbose = TRUE)

tsneTib <- tibble(perplexity = rep(tsneHyperPars$perplexity, each = 200),
                  theta      = rep(tsneHyperPars$theta, each = 200),
                  eta        = rep(tsneHyperPars$eta, each = 200),
                  max_iter   = rep(tsneHyperPars$max_iter, each = 200),
                  tSNE1      = unlist(map(tsne, ~.$Y[, 1])),
                  tSNE2      = unlist(map(tsne, ~.$Y[, 2])))

klTib <- mutate(tsneHyperPars, 
                KL = map(tsne, ~round(tail(.$itercosts, 1), 3)))

filter(tsneTib, eta == 200, max_iter == 1000) %>%
  ggplot(aes(tSNE1, tSNE2)) +
  facet_grid(theta ~ perplexity) +
  geom_text(data = filter(klTib, eta == 200, max_iter == 1000), 
            aes(label = KL), x = -80, y = -80) +
  geom_point() +
  theme_bw()
#ggsave("tsne1.pdf", width = 10, height = 6)

filter(tsneTib, perplexity == 30, theta == 0.5) %>%
  ggplot(aes(tSNE1, tSNE2)) +
  facet_grid(max_iter ~ eta) +
  geom_text(data = filter(klTib, perplexity == 30, theta == 0.5),
            aes(label = KL), x = -50, y = -40) +
  geom_point() +
  theme_bw()
#ggsave("tsne2.pdf", width = 10, height = 6)

# FINAL t-SNE ----
swissTsne <- select(swissTib, -Status) %>%
  Rtsne(perplexity = 30, theta = 0, max_iter = 5000, verbose = TRUE)

swissTibTsne <- swissTib %>%
  mutate_if(.funs = scale, .predicate = is.numeric, scale = FALSE) %>% # center variables
  mutate(tSNE1 = swissTsne$Y[, 1], tSNE2 = swissTsne$Y[, 2]) %>%
  gather(key = "Variable", value = "Value", c(-tSNE1, -tSNE2, -Status))

ggplot(swissTibTsne, aes(tSNE1, tSNE2, col = Value, shape = Status)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_gradient(low = "dark blue", high = "cyan") +
  theme_bw()

#ggsave("tsneOutput.pdf", width = 10, height = 7)

# UMAP ----
install.packages("umap")

library(umap)

# CREATE GRID OF UMAP HYPERPARAMETERS ----
umapHyperPars <- expand.grid(n_neighbors = seq(3, 19, 4),
                             min_dist    = seq(0.1, 0.5, 0.1),
                             metric      = c("euclidean", "manhattan"),
                             n_epochs    = seq(50, 400, 75))

umap <- pmap(umapHyperPars, umap, d = swissTib[, -1], verbose = TRUE)

umapTib <- tibble(n_neighbors = rep(umapHyperPars$n_neighbors, each = 200),
                  min_dist      = rep(umapHyperPars$min_dist, each = 200),
                  metric        = rep(umapHyperPars$metric, each = 200),
                  n_epochs   = rep(umapHyperPars$n_epochs, each = 200),
                  UMAP1      = unlist(map(umap, ~.$layout[, 1])),
                  UMAP2      = unlist(map(umap, ~.$layout[, 2])))

filter(umapTib, metric == "euclidean", n_epochs == 200) %>%
  ggplot(aes(UMAP1, UMAP2)) +
  facet_grid(n_neighbors ~ min_dist) +
  geom_point() +
  theme_bw()
#ggsave("umap1.pdf", width = 10, height = 6)

filter(umapTib, n_neighbors == 15, min_dist == 0.1) %>%
  ggplot(aes(UMAP1, UMAP2)) +
  facet_grid(metric ~ n_epochs) +
  geom_point() +
  theme_bw()
#ggsave("umap2.pdf", width = 10, height = 6)

# FINAL UMAP ----
swissUmap <- select(swissTib, -Status) %>%
  as.matrix() %>%
  umap(n_neighbors = 7, min_dist = 0.1,
       metric = "manhattan", n_epochs = 200, verbose = TRUE)

swissTibUmap <- swissTib %>%
  mutate_if(.funs = scale, .predicate = is.numeric, scale = FALSE) %>% # center variables
  mutate(UMAP1 = swissUmap$layout[, 1], UMAP2 = swissUmap$layout[, 2]) %>%
  gather(key = "Variable", value = "Value", c(-UMAP1, -UMAP2, -Status))

ggplot(swissTibUmap, aes(UMAP1, UMAP2, col = Value, shape = Status)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_gradient(low = "dark blue", high = "cyan") +
  theme_bw()
#ggsave("umapOutput.pdf", width = 10, height = 7)

# PROJECTING NEW DATA ONTO THE SCAFFOLD ----
predict(swissUmap, newBanknotes)

# EXERCISES ----
# 1
swissTib %>%
  mutate(tSNE1 = swissTsne$Y[, 1], tSNE2 = swissTsne$Y[, 2]) %>%
  gather(key = "Variable", 
         value = "Value", 
         c(-tSNE1, -tSNE2, -Status)) %>%
  ggplot(aes(tSNE1, tSNE2, col = Value, shape = Status)) +
  facet_wrap(~ Variable) +
  geom_point(size = 3) +
  scale_color_gradient(low = "dark blue", high = "cyan") +
  theme_bw()

# scaling is necessary because the scales of the variables are different
# from each other

# 2
umap3d <- select(swissTib, -Status) %>%
  as.matrix() %>%
  umap(n_neighbors = 7, min_dist = 0.1, n_components = 3,
       metric = "manhattan", n_epochs = 200, verbose = TRUE)

library(GGally)

ggpairs(as.data.frame(umap3d$layout), mapping = aes(col = swissTib$Status))
