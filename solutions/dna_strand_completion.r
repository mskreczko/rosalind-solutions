completeDNAStrand <- function(dnaseq) {
  dnaseq_vec <- strsplit(dnaseq, NULL)[[1]]
  reversed <- paste(rev(dnaseq_vec), collapse = "")
  reversed_vec <- strsplit(reversed, NULL)[[1]]

  for (i in 1:length(reversed_vec)) {
    if (reversed_vec[i] == "A") {
      reversed_vec[i] <- 'T'
    } else if (reversed_vec[i] == 'T') {
      reversed_vec[i] <- 'A'
    } else if (reversed_vec[i] == 'C') {
      reversed_vec[i] <- 'G'
    } else if (reversed_vec[i] == 'G') {
      reversed_vec[i] <- 'C'
    }
  }
  return(paste(reversed_vec, collapse=""))
}

completeDNAStrand(readLines("../data/rosalind_revc.txt"))