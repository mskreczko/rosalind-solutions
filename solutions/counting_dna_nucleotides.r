countNucleotides <- function(dna) {
  a <- 0
  c <- 0
  t <- 0
  g <- 0
  
  for (d in strsplit(dna, "")[[1]]) {
    if (d == 'A') {
      a <- a + 1
    } else if (d == 'C') {
      c <- c + 1
    } else if (d == 'G') {
      g <- g + 1
    } else if (d == 'T') {
      t <- t + 1
    }
  }
  print(a)
  print(c)
  print(g)
  print(t)
}

dna <- readLines('../data/rosalind_dna.txt')
countNucleotides(dna)