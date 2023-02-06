findLocations <- function(dnaseq, pattern) {
  locations <- c()
  for (i in 1:(nchar(dnaseq) - nchar(pattern))) {
    if (substr(dnaseq, i, i + nchar(pattern) - 1) == pattern) {
      locations <- c(locations, i)
    }
  }
  return(locations)
}

data = readLines("../data/rosalind_subs.txt")
findLocations(data[[1]], data[[2]])