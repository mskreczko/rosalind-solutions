computeHammingDistance <- function(s1, s2) {
  s1 <- strsplit(s1, "")[[1]]
  s2 <- strsplit(s2, "")[[1]]
  dist <- 0
  for (i in 1:length(s1)) {
    if (s1[i] != s2[i]) {
      dist = dist + 1
    }
  }
  return(dist)
}

data = readLines("../data/rosalind_hamm.txt")
computeHammingDistance(data[1], data[2])