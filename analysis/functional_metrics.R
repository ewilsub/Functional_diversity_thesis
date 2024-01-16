# CALCULATION OF FUNCTIONAL DIVERSITY METRICS

# 1. SETUP ----
library(ade4)
library(ape)
library(dplyr)
library(geometry)
library(picante)
library(usedist)
library(vegan)

set.seed(2023)

source('D:/Trabajo/R/FdivFunctions.R')
com_hel <- readRDS('data/com_hel.RDS')

traits_fuzzy <- readRDS('data/traits_fuzzy.RDS')

# 2. GENERATING FUNCTIONAL SPACE ----
ktab1_wf <- ktab.list.df(list(traits_fuzzy[which(rowSums(traits_fuzzy)!=0),])) 
func.dist <- dist.ktab(ktab1_wf, type = c("F"))
quality <- functional_quality(func.dist, nbdim = 15)
# 8 DIMENSIONS - BEST QUALITY


# generating functional space
func.space <- dudi.pco(func.dist, scan = F, nf = 8)

# saving data for ploting later
# quality
msd <- quality[["meanSD"]] %>% as.data.frame() %>% na.omit()
msd <- msd %>% mutate(dimension_n = seq(from = 2, to = 15, by = 1))
names(msd) <- c('mSD', 'dimension_n')
rownames(msd) <- NULL

## explained variance
variance <- data.frame(axis <- paste0("axis ", 1:8),
                       variance <- round(func.space$eig[1:8]/sum(func.space$eig),2))
names(variance) <- c('axis', 'variance') 

## trait correlations
## spearman rank correlations between original trait categories and axes
traits.func.space <- round(cor(traits_fuzzy[which(rowSums(traits_fuzzy)==6),],func.space$li, method="spearman"),2) %>% 
  as.data.frame()
colnames(traits.func.space) <- paste0("axis", 1:8)
traits.func.space <- traits.func.space %>% mutate(modality = rownames(traits.func.space),
                                                  trait = case_when(
                                                    grepl('^BOD', modality) ~ 'Body size',
                                                    grepl('^RESP', modality) ~ 'Respiration',
                                                    grepl('^FFG', modality) ~ 'FFG',
                                                    grepl('^RS', modality) ~ 'Reproductive stage',
                                                    grepl('^DISP', modality) ~ 'Dispersal',
                                                    grepl('^LOC', modality) ~ 'Locomotion',
                                                    grepl('^VOLT', modality) ~ 'Voltinism'),
                                                  .before = everything())
rownames(traits.func.space) <- NULL
##
taxa.func.space <- (func.space[["li"]])
taxa.func.space <- taxa.func.space %>% mutate(taxa = colnames(com_hel[,4:ncol(com_hel)]), .before = everything())
colnames(taxa.func.space) <- c('taxa', paste0("axis", 1:8))

func.space.info <- list(msd, variance, taxa.func.space, traits.func.space)
names(func.space.info) <- c('msd', 'variance', 'taxa', 'traits')
saveRDS(func.space.info, file = 'results/func_space_info.RDS')


# 3. CALCULATING FUNCTIONAL METRICS ----
# richness
chull <- chull_3d(func.space$li, m = 8, prec="Qt")
fric.obs <- fric(com_hel[,4:ncol(com_hel)], func.space$li, m = 8, prec = "QJ", fric.3d.max = chull)

# evenness
feve.obs <- feve(func.space$li, com_hel[,4:ncol(com_hel)], m = 8)

# dispersion
func.dist <- dist_setNames(func.dist, colnames(com_hel[,4:ncol(com_hel)]))
fdis.obs <- fdis(func.dist, com_hel[,4:ncol(com_hel)], m = 8)$FDis


# 4. CALCULATING SES OF FUNCTIONAL METRICS ----
n <- 999
ses_results <- data.frame(matrix(nrow = 51, ncol = 7))
names(ses_results) <- c('sample', 'fric', 'feve', 'fdis', 'fric.ses', 'feve.ses', 'fdis.ses')
ses_results$sample <- com_hel$sample
ses_results$fric <- fric.obs
ses_results$feve <- feve.obs
ses_results$fdis <- fdis.obs


# richness ----
fric.simulations <- data.frame(matrix(nrow = 51, ncol = n))
names(fric.simulations) <- paste0("simulation", 1:n)

for (sim in 1:n) 
  {
  traits.rand <- traits_fuzzy[sample(1:nrow(traits_fuzzy)),]
  rownames(traits.rand) <- rownames(traits_fuzzy)
  ktab1.rand <- ktab.list.df(list(traits.rand[which(rowSums(traits.rand)!=0),])) 
  func.dist.rand <- dist.ktab(ktab1.rand, type = c("F"))
  func.space.rand <- dudi.pco(func.dist.rand, scan = F, nf = 8)
  #com.rand <- randomizeMatrix(com_hel, null.model = "independentswap")
  fric.simulations[, sim] <- fric(com_hel[,4:ncol(com_hel)], func.space.rand$li, m = 8, prec = "QJ", fric.3d.max = chull)
}
ses.fric <- fric.obs - rowMeans(fric.simulations)
sd.sim <- apply(fric.simulations, 1, sd)
ses_results$fric.ses <- ses.fric/sd.sim


# evenness ----
feve.simulations <- data.frame(matrix(nrow = 51, ncol = n))
names(feve.simulations) <- paste0("simulation", 1:n)

for (sim in 1:n) 
{
  traits.rand <- traits_fuzzy[sample(1:nrow(traits_fuzzy)),]
  rownames(traits.rand) <- rownames(traits_fuzzy)
  ktab1.rand <- ktab.list.df(list(traits.rand[which(rowSums(traits.rand)!=0),])) 
  func.dist.rand <- dist.ktab(ktab1.rand, type = c("F"))
  func.space.rand <- dudi.pco(func.dist.rand, scan = F, nf = 8)
  feve.simulations[, sim]<- feve(func.space.rand$li, com_hel[,4:ncol(com_hel)], m = 8)
  
}
ses.feve <- feve.obs - rowMeans(feve.simulations)
sd.sim <- apply(feve.simulations, 1, sd)
ses_results$feve.ses <- ses.feve/sd.sim


# dispersion ----
fdis.simulations <- data.frame(matrix(nrow = 51, ncol = n))
names(fdis.simulations) <- paste0("simulation", 1:n)

for (sim in 1:n) 
{
  traits.rand <- traits_fuzzy[sample(1:nrow(traits_fuzzy)),]
  rownames(traits.rand) <- rownames(traits_fuzzy)
  ktab1.rand <- ktab.list.df(list(traits.rand[which(rowSums(traits.rand)!=0),])) 
  func.dist.rand <- dist.ktab(ktab1.rand, type = c("F"))
  func.dist.rand <- dist_setNames(func.dist.rand, colnames(com_hel[,4:ncol(com_hel)]))
  fdis.simulations[, sim]<- fdis(func.dist.rand, com_hel[,4:ncol(com_hel)], m = 8)$FDis
  
}
ses.fdis <- fdis.obs - rowMeans(fdis.simulations)
sd.sim <- apply(fdis.simulations, 1, sd)
ses_results$fdis.ses <- ses.fdis/sd.sim


# 5. SAVING RESULTS ----
saveRDS(ses_results, file = 'results/ses_results.RDS')


