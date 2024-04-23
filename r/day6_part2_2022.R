readAll <- function(path) {
  con <- file(path, "r")
  s <- readLines(con)
  close(con)
  return(paste(s, collapse = ""))
}

firstNUnique <- function(s, n) {
  for(i in n:nchar(s)) {
    b <- substr(s, i-n+1, i)
    if(length(unique(strsplit(b, "")[[1]])) == n) {
      return(i)
    }
  }
  return(-1)
}

s <- readAll("input.txt")
print(firstNUnique(s, 14))