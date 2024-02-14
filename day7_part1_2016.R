
data <- readLines("input.txt")
tlsCount <- 0

supportsTLS <- function(ip) {
  insideBrackets <- regmatches(ip, gregexpr("\\[[a-z]+\\]", ip))[[1]]
  
  for (bracketContent in insideBrackets) {
    if (grepl("([a-z])(?!\\1)([a-z])\\2\\1", bracketContent, perl=TRUE)) {
      return(FALSE)
    }
  }
  
  ip <- gsub("\\[[a-z]+\\]", "-", ip)
  return(grepl("([a-z])(?!\\1)([a-z])\\2\\1", ip, perl=TRUE))
}

for (line in data) {
  if (supportsTLS(line)) {
    tlsCount <- tlsCount + 1
  }
}

cat(tlsCount)
