# RLQ AND FOURTH CORNER ANALYSIS

# 1. SETUP
library(ade4)
library(dplyr)
library(vegan)

traits <- readxl::read_xlsx('data/traits.xlsx', sheet = 1)
env_log <- readRDS('data/env_log.RDS')
com_hel <- readRDS('data/com_hel.RDS')

# 2. COMMUNITY - COA ----
coa <- dudi.coa(com_hel %>% select(-sample, -site, -date), scannf = F, nf = 2)

# 3. ENVIRONMENT - PCA ----
pca <- dudi.pca(env_log %>% select(disc:no3_no2), row.w = coa$lw, scannf = F, nf = 2)

# 4. TRAITS - FCA ----
traits_fuzzy <- prep.fuzzy.var(traits[,-1], col.blocks = c(6,5,6,2,4,3), coa$cw)
fca <- dudi.fca(traits_fuzzy, scann = F, nf = 2)

#ordinations <- list(coa, pca, fca)
#names(ordinations) <- c('community', 'environment',  'traits')
#saveRDS(ordinations, file = 'rlq_pre_ordinations.RDS')

# 5. RLQ ----
rlq1 <- rlq(pca, coa, fca, scannf = F, nf = 2)
100*rlq1$eig/sum(rlq1$eig)

# Saving RLQ data for plotting
#rlq1[["lQ"]]$code <- 1:nrow(rlq1[["lQ"]])
#rlq.output <- list(rlq1[["l1"]], rlq1[["c1"]], rlq1[["lQ"]], rlq1[["lR"]])
#names(rlq.output) <- c('env', 'traits', 'spe', 'samples')
#saveRDS(rlq.output, file = 'rlq.RDS')

set.seed(2023)
randtest(rlq1, modeltype = 6, nrepet = 9999)

plot(rlq1)
s.arrow(rlq1$l1, boxes = FALSE, grid = FALSE) #variables
s.arrow(rlq1$c1, boxes = FALSE, grid = FALSE) #traits
s.label(rlq1$lQ, boxes = FALSE, grid = FALSE) #species

# 6. FOURTH CORNER ----
set.seed(2023)
traitsQaxes <- fourthcorner.rlq(rlq1, modeltype = 6,
                             typetest = "Q.axes", nrepet = 9999, 
                             p.adjust.method.G = "fdr",
                             p.adjust.method.D = "fdr")

plot(traitsQaxes, alpha = 0.05, type = "table", stat = "D2",
     col = c("white", "palegreen3", "red2"))
traitsQaxes$tabD2


set.seed(2023)
envRaxes <- fourthcorner.rlq(rlq1, modeltype = 6,
                          typetest = "R.axes", nrepet = 9999, 
                          p.adjust.method.G = "fdr",
                          p.adjust.method.D = "fdr")

plot(envRaxes, alpha = 0.05, type = "table",stat = "D2",
     col = c("white", "palegreen3", "red2"))
envRaxes$tabD2















