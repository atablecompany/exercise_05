NWScore <- function(sek1,sek2,match,mismatch,gap){
  m <- 1+length(sek1)
  n <- 1+length(sek2)
  S <- (0:(n-1))*gap
  #print(S)
  for (i in 2:m){
    s <- S[1]
    c <- S[1]+gap
    S[1] <- c
    for (j in 2:n){
      if (sek1[i-1]==sek2[j-1]){pom <- match} else {pom <- mismatch}
      c <- max(c(S[j]+gap, c+gap, s+pom))
      s <- S[j]
      S[j] <- c
    }
    #print(S)
  }
  return(S)
}


