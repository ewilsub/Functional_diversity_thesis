# functional_quality() ---------------------------------------------------------------------------------------------------
# function for computing the quality of functional dendrogramm and multidimensional functional spaces                      
# Maire et al. 2015                                                                                                                           
#
# Inputs:                                                                                                                           
# dist_funct: a species x species functional distance matrix. Object should be of class 'dist',                                                                                                                ##
#             for instance output of function 'daisy' (package 'cluster') or dist.ktab' from ade4 package.                                                                                                                ##
# nbdim: maximum number of dimensions for multidimensional functional spaces. By default, nbdim=7                   
#      		Final number of dimensions depends on the number of positive eigenvalues (after correction) obtained with PCoA     
# plot: character string to set the name of the jpeg file for plots illustrating the quality of functional spaces   
#                  NA means no plot                                                                                           
#                                                                                                                             
#     -high value for 'nbdim' can increase computation time                                                            
#     -if at least one trait is not numeric, 'metric' should be set as 'Gower'                                          
#     -if metric=Euclidean, functional traits are scaled (mean=0, sd=1) before computing functional distances           
#     -R libraries 'ape', 'clue', 'cluster', 'geometry', 'gtools' are required                                          
#                                                                                                                             
#                                                                                                                             
# Outputs:                                                                                             
# meanSD : a vector with mean squared deviation values for all the functional spaces tested							           
#            names are "t_clustering algorithm" for the best tree and 'm_kD' (with k=2:nbdim) for multidimensional spaces
# cuanto mas grande es, peor es el espacio funcional
#
# details_funct_space : a list with details about functional spaces                                                 
# mat_coord : coordinates of species in the nbdim multidimensional functional space (PCoA)                       
# upgma_tree : dendrogram built with the UPGMA clustering algorithm                                             
# dist_raw and dist_st : lists with raw and standardized distances between species in each functional space      
#                 distance matrices of functional spaces are as names in meanSD (e.g. $dist_raw$t_UPGMA or $dist_raw$m_3D)    
# a jpeg file in the current working directory with :                                                                 
#   a barplot showing the meanSD for all functional spaces                                               
#   'nbdim' panels (or only 15 if nbdim>15) illustrating the quality of each functional space            
#   Points represent species pairs. Mean squared deviation (mSD) is provided at the top of each panel.	 
#                                                                                                                             


functional_quality <- function( dist_funct,  nbdim=7,   plot="quality_funct_space") 
{
  
  #loading required libraries
  require(ape)
  require(clue)
  require(cluster)
  require(geometry)
  require(gtools)
  

  # checking data
  if ( ("dist" %in% class(dist_funct) ) ==FALSE )   {  stop(" 'dist_funct' must be of class 'dist'")     }
  if (length(dist_funct)<3)   {  stop(" there must be at least 3 species in 'dist_funct' ")     }
  if (sum(is.na(dist_funct))!=0)   {  stop(" NA are not allowed in 'dist_funct' ")     }
  if (nbdim<2)   {  stop(" 'nbdim' must be higher than 1")     }
  
  # functional distance 
  mat_dissim<-dist_funct
  
  # species names
  nm_sp<-row.names(as.matrix(dist_funct))
  
  # computing PCoA using Caillez correction
  mat_pcoa<-pcoa(mat_dissim, correction="none")
  
  # changing number of dimensions given number of positive eigenvalues
  nbdim<-min(nbdim,ncol(mat_pcoa$vectors) )
  
  # keeping species coordoinates on the 'nbdim' axes
  mat_coord<-mat_pcoa$vectors[,1:nbdim]
  row.names(mat_coord)<-nm_sp
  colnames(mat_coord)<-paste("PC",1:nbdim,sep="")
  
  # lists to store distance matrices
  dist_raw<-list()
  dist_st<-list()
  
  # computing Euclidean distances between species in the (nbdim-1) multidimensionnal functional spaces 
  for (k in 2:nbdim) 
  {
    eval(parse(text=paste("dist_",k,"D<-dist(mat_coord[,1:",k,"],method='euclidean')", sep="")))
    eval(parse(text=paste("dist_raw$m_",k,"D<-dist_",k,"D", sep="")))
  } # end of k
  
  # computing mean squared deviation between initial distance and standardized final distance in the functional space
  meanSD<-rep(NA,nbdim) ; names(meanSD)<-paste("m_",2:nbdim,"D",sep="")
  
  x<-mat_dissim # initial distance
  S<-length(nm_sp) # species richness
  
  # for muldimensionnal spaces
  for (k in 2:nbdim)  
  {
    eval(parse(text=paste("y<-dist_",k,"D",sep="")))
    yst<- y/max(y) * max(x)
    eval(parse(text=paste("dist_st$m_",k,"D<-dist_",k,"D", sep="")))
    meanSD[paste("m_",k,"D",sep="")]<-round( ( (sum((x-yst)^2)) / (S*(S-1)/2) ) ,6)
  }  # end of k
  
  # list of outputs
  res<-list(meanSD=meanSD, details_funct_space=list( mat_coord=mat_coord, dist_raw=dist_raw, dist_st=dist_st )  )
  
  # plotting change in meanSD with increasing number of dimensions  
  par(mfrow=c(1,1),mar=c(5,5,1,1))
  barplot(height=meanSD,names.arg=names(meanSD), xlab="Functional space", ylab= "Quality (Mean SD)", 
          space=0, cex.names=0.7, col=c(rep("grey88",nbdim) ) )
  
  abline(h=0.01,lwd=3,lty=2)
  
  # plotting quality of each functional space
  
  # functional distances
  x<-as.dist( mat_dissim) 
  

  invisible(res)
  
} # end of function functional_quality















#prep.fuzzy.df() ---------------------------------------------------------------------------------
# 
# Inputs:
# traits: fuzzy coding traits
# col.blocks: blocks indicating the number of trait categories per trait
#
# Outputs
# traits: fuzzy coding traits as percentages

prep.fuzzy.df<-function (traits, col.blocks) 
{
  if (!is.data.frame(traits)) 
    stop("Data.frame expected")
  if (sum(col.blocks) != ncol(traits)) {
    stop("Non convenient data in col.blocks")
  }
  if (is.null(names(col.blocks))) {
    names(col.blocks) <- paste("FV", as.character(1:length(col.blocks)), sep = "")
  }
  f1 <- function(x) {
    a <- sum(x)
    if (is.na(a)) 
      return(rep(0, length(x)))
    if (a == 0) 
      return(rep(0, length(x)))
    return(x/a)
  }
  k2 <- 0
  col.w <- rep(1, ncol(traits))
  
  for (k in 1:(length(col.blocks))) {
    k1 <- k2 + 1
    if (col.blocks[k]==1) k2<-k1 else k2 <- k2 + col.blocks[k]
    X <- as.matrix(traits[, k1:k2])
    if (col.blocks[k]==1) X[which(X[,1]>0),]<-1 else X <- t(apply(X, 1, f1))
    X.marge <- apply(X, 1, sum)
    X.marge <- X.marge
    X.marge <- X.marge/sum(X.marge)
    X.mean <- apply(X * X.marge, 2, sum)
    nr <- sum(X.marge == 0)
    cat(nr, "missing data found in block", k, "\n")
    traits[, k1:k2] <- X
    col.w[k1:k2] <- X.mean
  }
  attr(traits, "col.blocks") <- col.blocks
  attr(traits, "col.freq") <- col.w
  col.num <- factor(rep((1:length(col.blocks)), col.blocks))
  attr(traits, "col.num") <- col.num
  return(traits)
}

# chull_3d()  --------------------------------------------------------------------
# estimates the convex hull of a Functional Space
# Inputs:
# fpc: functional space
# m: number of axes to select
# prec: convex hull precision ("Qt" or "QJ")
#
# Output:
# a vector with the Functional Richness of each community

chull_3d<-function(fpc,m,prec=c("Qt","QJ")){
  
  convhulln(fpc[,1:m], c("FA",prec))$vol->fric.3d.max
  
  return(fric.3d.max)
}


# fric() ------------------------------------------------------------------------------
# estimates the Functional Richness of a set of communties
# This function computes the hypervolume to estimate how each community fills
# the functional space
#
# Inputs:
# taxa: community data
# fpc: functional space
# m: number of axes to select
# prec: convex hull precision ("Qt" or "QJ")
# fric.3d.max: volume of the regional convex hull (set fric.3d.max=1 to have the absolute FRic value)
#
# Output:
# a vector with the Functional Richness of each community

fric<-function(taxa,fpc,m,prec=c("Qt","QJ"), fric.3d.max=NULL){
  fric.3d<-rep(NA,nrow(taxa))
  names(fric.3d)<-rownames(taxa)
  
  # Convex hull of regional pool
  if(is.null(fric.3d.max)==T) convhulln(fpc[,1:m], c("FA",prec))$vol->fric.3d.max
  
  specnumber(taxa)->ric
  
  for (com in 1:nrow(taxa)){
    fpc[which(unlist(rep(taxa[com,]))>0),1:m]->tr.com
    if (ric[com]>=m+1) convhulln(tr.com, c("FA",prec))$vol/fric.3d.max->fric.3d[com] else NA->fric.3d[com]
  }
  return(fric.3d)
}

# fric_1d() estimates the Functional Richness of a set of communties
# This function computes the hypervolume to estimate how each community fills
# the functional space
#
# Inputs:
# taxa: community data
# fpc: functional space
# m: number of axes to select
#
# Output:
# a vector with the Functional Richness of each community

fric_1d<-function(taxa,fpc,m){
  
  fric.1d<-rep(NA,nrow(taxa))
  names(fric.1d)<-rownames(taxa)
  fric.1d.max<-rep(NA,m)
  
  for (i in 1:m) sum(abs(range(fpc[,i])))->fric.1d.max[i]
  
  specnumber(taxa)->ric
  for (com in 1:nrow(taxa)){
    fpc[which(unlist(rep(taxa[com,]))>0),1:m]->tr.com
    
    if (ric[com]>=1) mean(sum(abs(range(tr.com)))/fric.1d.max)->fric.1d[com] else NA->fric.1d[com]
  }
  return(fric.1d)
}







# feve() -------------------------------------------------------------------------
# estimates the Functional Evenness of a set of communties
# This function computes regularity in the distribution of taxa or abundances
# across the Functional Space, using the Spanning Tree Method
# 
# Function modified from Lalibert? & Legendre (2010) Ecology
#
# Inputs:
# fpc: functional space axes
# taxa: community data
# m: number of axes to select
#
# Output:
# FEve: a vector with the Functional Evenness of each community

feve<-function(fpc,taxa,m){
  
  rdt=1
  
  # Creating variables
  nrow(taxa)->c
  FEve <- rep(NA, c) ; names(FEve) <- row.names(taxa)
  nb.sp<-specnumber(taxa)
  
  # generating the taxonomic matrix arranged according to the replicated trait values
  tax.pool<-ncol(taxa)
  taxa.rep<-data.frame(matrix(NA,nrow(taxa),tax.pool*rdt))
  spp.list<-c(1:(tax.pool*rdt))
  
  for (spp in 1:tax.pool) {paste(rep("spp",rdt),spp,sep="")->spp.list[((spp-1)*rdt+1):(spp*rdt)]}
  
  colnames(taxa.rep)<-spp.list
  
  for (spp in 1:tax.pool){taxa.rep[,((spp-1)*rdt+1):(spp*rdt)]<-taxa[,spp]/rdt}                             
  
  # Estimating Functional Evenness for each community
  
  for (i in 1:c) {
    sppres <- which(taxa.rep[i, ] > 0)
    # number of species in the community
    S <- length(sppres)
    ab <- as.matrix(taxa.rep[i, sppres])
    # scaling of abundances
    abundrel <- ab / sum(ab)
    
    # selecting the c
    tr <- data.frame(fpc[sppres,1:m ])
    
    if (nb.sp[i] > 2) {
      tr.dist <- dist(tr)
      linkmst <- mst(tr.dist)
      mstvect <- as.dist(linkmst)
      abund2 <- matrix(0, nrow = S, ncol = S)
      for (q in 1:S) for (r in 1:S) abund2[q, r] <- abundrel[q] +  abundrel[r]
      abund2vect <- as.dist(abund2)
      EW <- rep(0, S - 1)
      flag <- 1
      for (mv in 1:((S - 1) * S/2)) {
        if (mstvect[mv] != 0) {
          EW[flag] <- tr.dist[mv]/(abund2vect[mv])
          flag <- flag + 1
        }
      }
      minPEW <- rep(0, S - 1)
      OdSmO <- 1/(S - 1)
      for (l in 1:(S - 1)) minPEW[l] <- min((EW[l]/sum(EW)), 
                                            OdSmO)
      FEve[i] <- ((sum(minPEW)) - OdSmO)/(1 - OdSmO)
    } else FEve[i] <- NA
  }
  return(FEve)
}


# fdis() ------------------------------------------------------------------------- 
# estimates the Functional Dispersion of a set of communties
# This function computes the weighted mean distance to community centroid in
# the functional space
# 
# Function modified from Lalibert? & Legendre (2010) Ecology
#
# Inputs:
# d: trait dissimilarity matrix
# a: community data
# m: number of axes to select
# tol: tolerance threshold to test whether the distance matrix is Euclidean
#
# Output:
# FDis: a vector with the Functional Dispersion of each community
# eig: eigenvectors of each functional axis
# vectors: functional axes

fdis<-function (d, a, m, tol = 1e-07) 
{
  if (!inherits(d, "dist")) 
    stop("'d' must be a 'dist' object.")
  n <- attr(d, "Size")
  if (is.null(attr(d, "Labels"))) 
    stop("'d' must have labels.", "\n")
  else sn.d <- attr(d, "Labels")
  if (missing(a)) {
    ab.names <- list("Community1", sn.d)
    a <- matrix(1, 1, n, dimnames = ab.names)
  }
  com <- nrow(a)
  if (ncol(a) != n) 
    stop("Number of columns in 'a' must be equal to the number of objects in 'd'.")
  if (is.null(colnames(a))) 
    stop("'a' must have column names", "\n")
  else sn.a <- colnames(a)
  if (any(sn.d != sn.a)) 
    stop("Species labels in 'd' and 'a' need to be identical and ordered alphabetically (or simply in the same order).", 
         "\n")
  a[which(is.na(a))] <- 0
  abun.sum <- apply(a, 1, sum)
  if (any(abun.sum == 0)) 
    warning("At least one community has zero-sum abundances (no species).", 
            "\n")
  abun.sum2 <- apply(a, 2, sum)
  if (any(abun.sum2 == 0)) 
    warning("At least one species does not occur in any community (zero total abundance across all communities).", 
            "\n")
  if (any(is.na(d))) 
    stop("NA's in the distance matrix.", "\n")
  A <- matrix(0, ncol = n, nrow = n)
  A[row(A) > col(A)] <- -0.5 * d^2
  A <- A + t(A)
  G <- bicenter.wt(A)
  e <- eigen(G, symmetric = TRUE)
  vectors <- e$vectors
  eig <- e$values
  w0 <- eig[n]/eig[1]
  if (w0 > -tol) 
    r <- sum(eig > (eig[1] * tol))
  else r <- length(eig)
  vectors <- vectors[, 1:r, drop = FALSE] %*% diag(sqrt(abs(eig <- eig[1:r])), 
                                                   r)
  dimnames(vectors) <- list(colnames(a), NULL)
  pos <- eig > 0
  if (m>0) pos<-c(pos[1:m],rep(F,length(pos)-m))
  
  avg.dist.cent <- rep(NA, nrow(a))
  names(avg.dist.cent) <- row.names(a)
  for (i in 1:com) {
    pres <- which(a[i, ] > 0)
    nb.sp <- nrow((unique(vec <- vectors[pres, , drop = F])))
    if (nb.sp >= 2) {
      w <- a[i, pres]
      centroid <- apply(vec, 2, weighted.mean, w = w)
      dist.pos <- sweep(vec[, pos, drop = F], 2, centroid[pos])
      dist.pos <- rowSums(dist.pos^2)
      if (any(!pos)) {
        dist.neg <- sweep(vec[, !pos, drop = F], 2, centroid[!pos])
        dist.neg <- rowSums(dist.neg^2)
      }
      else dist.neg <- 0
      zij <- sqrt(abs(dist.pos - dist.neg))
      avg.dist.cent[i] <- weighted.mean(zij, w)
    }
    else avg.dist.cent[i] <- 0
  }
  return(list(FDis = avg.dist.cent, eig = eig, vectors = vectors))
}


# null.func.test() ----------------------------------------------------------
# is an ad hoc function that estimates the null distribution for 
# FRic, FDis and FEve
#
# Inputs:
# taxa: community data
# traits: a matrix with the functional trait types
# runs: number of randomizations
# m: functional axes to select (For FDis and FEve)
# m: functional axes to select For FRic
# prec: method to estimate the convex hull (Qt or QJ)

# Outputs:
#
# ses and pvalue for each model coefficient

null.func.test<-function(taxa,traits,runs=9,m, m_fric, prec="Qt",fric_type="quad"){
  
  res<-list()
  
  # Calculation of the GLOBAL functional distances between species
  
  ktab1_wf <- ktab.list.df(list(traits[which(rowSums(traits)!=0),]))
  tr.dist <- dist.ktab(ktab1_wf, type= c("F"))
  
  dudi.pco(tr.dist,nf=m,scannf = F)->tr.pco
  
  funspace <- tr.pco$li
  
  # Using Global PCoA
  chull_3d(funspace,m=m_fric,prec=prec) -> ch_ov
  fric(taxa,funspace,m_fric,prec=prec, fric.3d.max = ch_ov) -> FRic_g # global
  fdisp_k_sub(tr.dist, taxa,tax_sub=colnames(taxa), m)$FDis -> FDis_g # global
  feve_k(funspace,taxa,m) -> FEve_g # global
  
  #if(min(FRic_g, na.rm=T)==0) log(FRic_g+0.01)->FRic_g else log(FRic_g)->FRic_g
  
  sqrt(FRic_g)->FRic_g
  
  # Observed models
  if(fric_type=="quad") mod_fric_g<-lm(FRic_g~lat+I(lat^2)) else mod_fric_g<-lm(FRic_g~lat)
  mod_fdis_g<-lm(FDis_g~lat+I(lat^2))
  mod_feve_g<-lm(FEve_g~lat+I(lat^2))
  
  # Saving observed results
  res$obs.fric<-res$obs.fdis<-res$obs.feve<-rep(NA,3)
  
  names(res$obs.fric)<-names(res$obs.fdis)<-names(res$obs.feve)<-c("intercept","lat","lat^2")
  
  coef(mod_fric_g)->res$obs.fric
  coef(mod_fdis_g)->res$obs.fdis
  coef(mod_feve_g)->res$obs.feve
  
  # Creating an empty matrix for null model coefficients
  res$null.fric<-res$null.fdis<-res$null.feve<-data.frame(matrix(NA,runs,3))
  
  colnames(res$null.fric)<-colnames(res$null.fdis)<-colnames(res$null.feve)<-c("intercept","lat","lat^2")
  
  for (i in 1:runs){
    sample(1:nrow(traits))->ran
    traits[ran,]->traits.r # randomizing traits
    rownames(traits.r)<-rownames(traits)
    
    # Calculation of the GLOBAL functional distances between species
    ktab1_wf <- ktab.list.df(list(traits.r[which(rowSums(traits.r)==9),]))
    tr.dist.r <- dist.ktab(ktab1_wf, type= c("F"))
    
    dudi.pco(tr.dist.r,nf=m,scannf = F)->tr.pco.r
    
    funspace.r <- tr.pco.r$li
    
    fric(taxa, funspace.r,m_fric,prec=prec, fric.3d.max = ch_ov) -> FRic_g_null # global null
    fdisp_k_sub(tr.dist.r, taxa,tax_sub=colnames(taxa), m) -> FDis_g_null # global null
    feve_k(funspace.r,taxa,m) -> FEve_g_null # global null
    
    FRic_g_null[which(is.na(FRic_g_null)==T)]<-0
    FEve_g_null[which(is.na(FEve_g_null)==T)]<-0
    
    sqrt(FRic_g_null)->FRic_g_null
    
    # Null models
    if(fric_type=="quad") mod_fric_g_null<-lm(FRic_g_null~lat+I(lat^2)) else mod_fric_g_null<-lm(FRic_g_null~lat)
    mod_fdis_g_null<-lm(FDis_g_null$FDis~lat+I(lat^2))
    mod_feve_g_null<-lm(FEve_g_null~lat+I(lat^2))
    
    coef(mod_fric_g_null)->res$null.fric[i,1:length(coef(mod_fric_g_null))]
    coef(mod_fdis_g_null)->res$null.fdis[i,]
    coef(mod_feve_g_null)->res$null.feve[i,]
    
    cat("Randomization number", i, "\n")
  }
  
  pval.res<-matrix(NA,9,ncol=2)
  colnames(pval.res)<-c("SES","pval")
  rownames(pval.res)<-c("FRic-intercept", "FRic-lat", "FRic-lat^2",
                        "FDis-intercept","FDis-lat","FDis-lat^2",
                        "FEve-intercept","FEve-lat","FEve-lat^2")
  
  # SES
  p.val(res$obs.fric[1],res$null.fric[,1])$z.score->pval.res[1,1]
  p.val(res$obs.fric[2],res$null.fric[,2])$z.score->pval.res[2,1]
  if(fric_type=="quad") p.val(res$obs.fric[3],res$null.fric[,3])$z.score->pval.res[3,1]
  
  p.val(res$obs.fdis[1],res$null.fdis[,1])$z.score->pval.res[4,1]
  p.val(res$obs.fdis[2],res$null.fdis[,2])$z.score->pval.res[5,1]
  p.val(res$obs.fdis[3],res$null.fdis[,3])$z.score->pval.res[6,1]
  
  p.val(res$obs.feve[1],res$null.feve[,1])$z.score->pval.res[7,1]
  p.val(res$obs.feve[2],res$null.feve[,2])$z.score->pval.res[8,1]
  p.val(res$obs.feve[3],res$null.feve[,3])$z.score->pval.res[9,1]
  
  # p-value
  p.val(res$obs.fric[1],res$null.fric[,1])$p.value->pval.res[1,2]
  p.val(res$obs.fric[2],res$null.fric[,2])$p.value->pval.res[2,2]
  if(fric_type=="quad") p.val(res$obs.fric[3],res$null.fric[,3])$p.value->pval.res[3,2]
  
  p.val(res$obs.fdis[1],res$null.fdis[,1])$p.value->pval.res[4,2]
  p.val(res$obs.fdis[2],res$null.fdis[,2])$p.value->pval.res[5,2]
  p.val(res$obs.fdis[3],res$null.fdis[,3])$p.value->pval.res[6,2]
  
  p.val(res$obs.feve[1],res$null.feve[,1])$p.value->pval.res[7,2]
  p.val(res$obs.feve[2],res$null.feve[,2])$p.value->pval.res[8,2]
  p.val(res$obs.feve[3],res$null.feve[,3])$p.value->pval.res[9,2]
  
  res$pval.res<-pval.res
  
  return(res)
}






