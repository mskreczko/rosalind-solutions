createDnaMatrix <- function(dnaStrings) {
  return(matrix(data=dnaStrings, nrow=length(dnaStrings), ncol=1))
}

createProfileMatrix <- function(dnaStrings, dnaMatrix) {
  profile <- matrix(0, nrow=4, ncol=nchar(dnaStrings[[1]]))
  for(i in 1:nchar(dnaStrings[[1]])) {
    for (dna in dnaMatrix) {
      symbol <- substr(dna, i, i)
      if (symbol == "A") {
        profile[1, i] <- profile[1, i] + 1
      } else if (symbol == "C") {
        profile[2, i] <- profile[2, i] + 1
      } else if (symbol == "G") {
        profile[3, i] <- profile[3, i] + 1
      } else if (symbol == "T") {
        profile[4, i] <- profile[4, i] + 1
      }
    }
  }
  
  return(profile)
}

createConsensusString <- function(profileMatrix) {
  consensusVector <- c()
  for (i in 1:length(profileMatrix[1,])) {
    maxIdx <- which.max(profileMatrix[, i])
    if (maxIdx == 1) {
      consensusVector <- c(consensusVector, "A")
    } else if (maxIdx == 2) {
      consensusVector <- c(consensusVector, "C")
    } else if (maxIdx == 3) {
      consensusVector <- c(consensusVector, "G")
    } else if (maxIdx == 4) {
      consensusVector <- c(consensusVector, "T")
    }
  }
  
  return(paste(consensusVector, collapse=""))
}

data <- readLines("../data/rosalind_cons.txt")

dnaStrings <- c()
for (i in 1:length(data)) {
  if (grepl(">Rosalind_", data[i])) {
    tmp <- c()
    for (j in (i + 1):length(data)) {
      if (!grepl(">Rosalind_", data[j]) && j != length(data)) {
        tmp <- c(tmp, data[j])
      } else {
        dnaStrings <- c(dnaStrings, paste(tmp, collapse=""))
        break
      }
    }
  }
}

dnaMatrix <- createDnaMatrix(dnaStrings)
profileMatrix <- createProfileMatrix(dnaStrings, dnaMatrix)
consensusString <- createConsensusString(profileMatrix)
print(consensusString)