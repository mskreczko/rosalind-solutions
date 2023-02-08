getExpectedOffspring <- function(populations) {
  p <- c(1, 1, 1, 0.75, 0.5, 0)  # probability of offspring displaying the dominant phenotype

  for (i in 1:length(p)) {
    p[[i]] <- p[[i]] * 2 # every couple has exactly two offspring
  }
  
  probability <- 0
  for (i in 1:length(populations)) {
    probability <- probability + (p[[i]] * populations[[i]])
  }
  return(probability)
}

data = readLines("../data/rosalind_iev.txt")
getExpectedOffspring(as.integer(unlist(strsplit(data, " "))))