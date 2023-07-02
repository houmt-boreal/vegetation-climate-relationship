library(mixOmics)
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
Y<-Y[1]
toxicity.spls <- spls(X, Y, ncomp = 1)
ex = explained_variance(toxicity.spls$Y, toxicity.spls$variates$Y, ncomp =1)
ex2 = explained_variance(toxicity.spls$Y, toxicity.spls$variates$X, ncomp =1)
