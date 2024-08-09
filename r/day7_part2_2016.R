supportsSSL <- function(ip) {
  bracketContents <- regmatches(ip, gregexpr("\\[[a-z]+\\]", ip))
  bracketContents <- unlist(bracketContents)
  
  ip <- gsub("\\[[a-z]+\\]", "-", ip)
  abas <- findABAs(ip)
  
  for (aba in abas) {
    bab <- paste0(substr(aba, 2, 2), substr(aba, 1, 1), substr(aba, 2, 2))
    if (any(grepl(bab, bracketContents))) return(TRUE)
  }
  FALSE
}

findABAs <- function(s) {
  abas <- c()
  for (i in 1:(nchar(s) - 2)) {
    if (substr(s, i, i) != substr(s, i + 1, i + 1) && 
        substr(s, i, i) == substr(s, i + 2, i + 2)) {
      abas <- c(abas, substr(s, i, i + 2))
    }
  }
  abas
}

countSSL <- function(filename) {
  sslCount <- 0
  lines <- readLines(filename)
  for (line in lines) {
    if (supportsSSL(line)) sslCount <- sslCount + 1
  }
  sslCount
}

cat(countSSL("input.txt"), "\n")