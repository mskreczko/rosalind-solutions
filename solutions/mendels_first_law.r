getProbability <- function(k, m, n) {
  p <- k + m + n
  prob <- ((k ** 2 - k) + (2 * k * m) + (3 / 4 * (m ** 2 - m)) + (2* k * n) + (m * n)) / (p * (p - 1))
  return(prob)
}

data = readLines("../data/rosalind_iprb.txt")
data = strsplit(data, split=" ")[[1]]
getProbability(strtoi(data[[1]]), strtoi(data[[2]]), strtoi(data[[3]]))