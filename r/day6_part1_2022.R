readAll <- function(path) {
  con <- file(path, "r")
  s <- readLines(con)
  close(con)
  return(paste(s, collapse = ""))
}

firstNUnique <- function(s, n) {
  for(i in n:nchar(s)) {
    b <- strsplit(substr(s, i-n+1, i), "")[[1]]
    if(length(b) == length(unique(b))) {
      return(i)
    }
  }
  return(-1)
}

s <- readAll("input.txt")
print(firstNUnique(s, 4))