Hirschberg = function(X, Y, align=list(DNAString(),DNAString()), match, mismatch, gap){
  Z = align[[1]]
  W = align[[2]]
  if (length(X) == 0) {
    for (i in 1:length(Y)){
      Z = paste0(Z, '-')
      W = paste0(W, Y[i])
    }
    align = paste0(Z, W)
  }
  else if (length(Y) == 0){
    for (i in 1:length(X)){
      Z = paste0(Z, X[i])
      W = paste0(W,'-')
    }
    align = paste0(Z, W)
  }
  else if (length(X) == 1 && length(Y) == 1){
    Z = paste0(Z, X[1])
    W = paste0(W, Y[1])
    align = paste0(Z, W)
  }
  else{
    xlen = length(X)
    xmid = xlen / 2
    ylen = length(Y)
    ScoreL = NWScore(X[1:xmid], Y, match, mismatch, gap)
    ScoreR = NWScore(reverse(X[xmid+1:xlen]), reverse(Y))
    ymid = arg
    align = Hirschberg(X[1:xmid], Y[1:ymid], align, match, mismatch, gap)
    align = Hirschberg(X[xmid+1:xlen], Y[ymid+1:ylen], match=match, mismatch=mismatch, gap=gap)
  }
  return(align)
}

library(Biostrings)
source('NWScore.R')

X = DNAString('AGTCGCA')
Y = DNAString('TATGC')
Hirschberg(X, Y, match=2, mismatch=-1, gap=-2)

