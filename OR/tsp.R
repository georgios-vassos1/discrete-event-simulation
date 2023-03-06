
tsp <- function(i, mask) {

  if (mask == bitwOr(bitwShiftL(1L,i), 3L)) {
    return(dist[1,i])
  }

  if (memo[i,mask] != -1L) {
    return(memo[i,mask])
  }

  res <- Inf
  for (j in seq(n)) {
    if ((bitwAnd(mask,bitwShiftL(1L,j)) != 0L) && (j != i) && (j != 1L)) {
      res <- min(res, dist[j,i] + tsp( j, bitwAnd(mask,bitwNot(bitwShiftL(1L,i))) ))
    }
  }
  memo[i,mask] <- res
  res
}


