computeGcContent <- function(dnaseq) {
  c = 0
  for (s in strsplit(dnaseq, "")[[1]]) {
    if (s == 'G' || s == 'C') {
      c = c + 1
    }
  }
  return(c/nchar(dnaseq)*100)
}

dataToFastaFrame <- function(data) {
  ids <- c()
  dnaseqs <- c()
  for (i in 1:length(data)) {
    if (grepl(">Rosalind_", data[i])) {
      ids <- c(ids, data[i])
      tmp <- c()
      for (j in (i + 1):length(data)) {
        if (!grepl(">Rosalind_", data[j]) && j != length(data)) {
          tmp <- c(tmp, data[j])
        } else {
          dnaseqs <- c(dnaseqs, paste(tmp, collapse=""))
          break
        }
      }
    }
  }
  return(data.frame(ids, dnaseqs))
}

findBestGcContent <- function(fastaFrame) {
  bestContent = computeGcContent(fastaFrame[1, 2])
  bestId = fastaFrame[1, 1]
  for (i in 2:nrow(fastaFrame)) {
    x <- computeGcContent(fastaFrame[i, 2])
    if (x > bestContent) {
      bestContent <- x
      bestId <- fastaFrame[i, 1]
    }
  }
  print(bestId)
  print(bestContent)
  return(bestContent)
}

data = readLines("../data/rosalind_gc.txt")
df = dataToFastaFrame(data)
findBestGcContent(df)
