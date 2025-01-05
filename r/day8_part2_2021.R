
jumbledSevenSegment <- function(input) {
  lines <- strsplit(input, "\n")[[1]]
  parsedInput <- lapply(lines, function(line) {
    parts <- unlist(regmatches(line, gregexpr("[a-g]+", line)))
    if (length(parts) != 14) {
      stop(paste("should be 14 parts in each input line, got", length(parts)))
    }
    sapply(parts, function(x) paste(sort(strsplit(x, "")[[1]]), collapse = ""))
  })
  
  ans <- 0
  for (set in parsedInput) {
    workingSet <- set[1:10]
    indexToCharacters <- character(10)
    
    for (i in seq_along(workingSet)) {
      mapping <- workingSet[i]
      len <- nchar(mapping)
      if (len == 2) {
        indexToCharacters[2] <- mapping
      } else if (len == 4) {
        indexToCharacters[5] <- mapping
      } else if (len == 3) {
        indexToCharacters[8] <- mapping
      } else if (len == 7) {
        indexToCharacters[9] <- mapping
      }
    }
    
    workingSet <- workingSet[!nchar(workingSet) %in% c(2, 3, 4, 7)]
    
    zeroThreeOrNine <- workingSet[sapply(workingSet, function(x) checkStringOverlap(x, indexToCharacters[2]))]
    
    if (length(zeroThreeOrNine) != 3) {
      stop(paste("one three or nine does not have three matches: got", length(zeroThreeOrNine)))
    }
    
    indexToCharacters[4] <- zeroThreeOrNine[nchar(zeroThreeOrNine) == 5]
    zeroThreeOrNine <- zeroThreeOrNine[nchar(zeroThreeOrNine) != 5]
    
    indexToCharacters[10] <- zeroThreeOrNine[sapply(zeroThreeOrNine, function(x) checkStringOverlap(x, indexToCharacters[5]))]
    zeroThreeOrNine <- zeroThreeOrNine[!sapply(zeroThreeOrNine, function(x) checkStringOverlap(x, indexToCharacters[5]))]
    
    indexToCharacters[1] <- zeroThreeOrNine
    
    workingSet <- workingSet[!sapply(workingSet, function(x) checkStringOverlap(x, indexToCharacters[2]))]
    
    if (length(workingSet) != 3) {
      stop(paste("expected length of 3 at this stage, got", length(workingSet)))
    }
    
    indexToCharacters[7] <- workingSet[nchar(workingSet) == 6]
    workingSet <- workingSet[nchar(workingSet) != 6]
    
    indexToCharacters[6] <- workingSet[sapply(workingSet, function(x) checkStringOverlap(indexToCharacters[10], x))]
    workingSet <- workingSet[!sapply(workingSet, function(x) checkStringOverlap(indexToCharacters[10], x))]
    
    if (length(workingSet) != 1) {
      stop(paste("expected length of 1 at this stage, got", length(workingSet)))
    }
    
    indexToCharacters[3] <- workingSet
    
    num <- 0
    for (out in set[11:14]) {
      num <- num * 10 + which(sapply(indexToCharacters, function(x) identical(x, out))) -1
    }
    ans <- ans + num
  }
  ans
}

checkStringOverlap <- function(larger, smaller) {
  if (nchar(larger) < nchar(smaller)) {
    temp <- larger
    larger <- smaller
    smaller <- temp
  }
  all(strsplit(smaller, "")[[1]] %in% strsplit(larger, "")[[1]])
}

input <- paste(readLines("input.txt"), collapse = "\n")
result <- jumbledSevenSegment(input)
cat(result, "\n")
