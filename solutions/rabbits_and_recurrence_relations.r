rabbits <- function(n, k) {
  if (n == 0) {
    return(0)
  } else if (n == 1) {
    return(1)
  }
  
  return(rabbits(n - 1, k) + k * rabbits(n - 2, k))
}

ln <- strsplit(readLines("../data/rosalind_fib.txt"), " ")[[1]]
rabbits(strtoi(ln[1]), strtoi(ln[2]))