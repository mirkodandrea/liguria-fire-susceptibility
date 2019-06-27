# Order factor levels with PCA approach 
# Reference: Coppersmith, D., Hong, S.J. & Hosking, J.R. (1999) Partitioning Nominal Attributes in Decision Trees. Data Min Knowl Discov 3:197. \url{https://doi.org/10.1023/A:1009869804967}.

pca.order <- function(y, x) {
  x <- droplevels(x)
  if (nlevels(x) < 2) {
    return(as.character(levels(x)))
  }
  
  ## Create contingency table of the nominal outcome with the nominal covariate
  N <- table(x, droplevels(y))
  
  
  ## PCA of weighted covariance matrix of class probabilites
  P <- N/rowSums(N)
  S <- cov.wt(P, wt = rowSums(N))$cov
  pc1 <- prcomp(S, rank. = 1)$rotation
  score <- P %*% pc1
  
  ## Return ordered factor levels
  levels(x)[order(score)]
}