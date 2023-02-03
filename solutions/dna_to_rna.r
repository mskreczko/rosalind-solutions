dnaToRna <- function(dnaseq) {
  return(gsub("T", "U", dnaseq))
}

dnaseq = readLines('../data/rosalind_rna.txt')
dnaToRna(dnaseq)