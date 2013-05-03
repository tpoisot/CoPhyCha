library(ape)

nsp = 40

tH = rtree(nsp) #random tree with 40 leaves
tP = tH

imat = matrix(ncol=2, nrow=nsp)
imat[,1] = imat[,2] = tH$tip.label

cophyloplot(tH, tP, assoc=imat, length.line=4, space=28, gap=3)

parafit(tH,tP,imat)