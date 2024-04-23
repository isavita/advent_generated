isValidPassword <- function(password) {
  s <- as.character(password)
  hasDouble <- FALSE
  
  for(i in 1:(nchar(s) - 1)) {
    if(substring(s, i, i) > substring(s, i + 1, i + 1)) {
      return(FALSE)
    }
    if(substring(s, i, i) == substring(s, i + 1, i + 1)) {
      if((i == 1 || substring(s, i, i) != substring(s, i - 1, i - 1)) && 
         (i + 2 > nchar(s) || substring(s, i, i) != substring(s, i + 2, i + 2))) {
        hasDouble <- TRUE
      }
    }
  }
  
  return(hasDouble)
}

input <- readLines("input.txt")
rangeStr <- gsub("\\s+", "-", input)
ranges <- strsplit(rangeStr, "-")[[1]]
start <- as.integer(ranges[1])
end <- as.integer(ranges[2])

count <- 0
for(i in start:end) {
  if(isValidPassword(i)) {
    count <- count + 1
  }
}

print(count)