codonTable <- c(
  "UUU"= "F",     "CUU"= "L",     "AUU"= "I",     "GUU"= "V",
  "UUC"= "F",     "CUC"= "L",     "AUC"= "I",     "GUC"= "V",
  "UUA"= "L",     "CUA"= "L",     "AUA"= "I",     "GUA"= "V",
  "UUG"= "L",     "CUG"= "L",     "AUG"= "M",     "GUG"= "V",
  "UCU"= "S",     "CCU"= "P",     "ACU"= "T",     "GCU"= "A",
  "UCC"= "S",     "CCC"= "P",     "ACC"= "T",     "GCC"= "A",
  "UCA"= "S",     "CCA"= "P",     "ACA"= "T",     "GCA"= "A",
  "UCG"= "S",     "CCG"= "P",     "ACG"= "T",     "GCG"= "A",
  "UAU"= "Y",     "CAU"= "H",     "AAU"= "N",     "GAU"= "D",
  "UAC"= "Y",     "CAC"= "H",     "AAC"= "N",     "GAC"= "D",
  "UAA"= "Stop",  "CAA"= "Q",     "AAA"= "K",     "GAA"= "E",
  "UAG"= "Stop",  "CAG"= "Q",     "AAG"= "K",     "GAG"= "E",
  "UGU"= "C",     "CGU"= "R",     "AGU"= "S",     "GGU"= "G",
  "UGC"= "C",     "CGC"= "R",     "AGC"= "S",     "GGC"= "G",
  "UGA"= "Stop",  "CGA"= "R",     "AGA"= "R",     "GGA"= "G",
  "UGG"= "W",     "CGG"= "R",     "AGG"= "R",     "GGG"= "G"
)

translateRNAToProtein <- function(rna) {
  rna <- strsplit(rna, "")[[1]]
  print(class(rna))
  protein <- c()
  for (i in seq(1, length(rna), by = 3)) {
    symbol <- codonTable[paste(rna[c(i, i + 1, i + 2)], collapse="")]
    if (symbol == "Stop") {
      break
    }
    protein <- c(protein, symbol)
  }
  return(paste(protein, collapse=""))
}

translateRNAToProtein(readLines("../data/rosalind_prot.txt"))