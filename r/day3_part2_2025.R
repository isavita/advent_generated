
add_big <- function(s1, s2) {
  v1 <- as.integer(rev(strsplit(s1, "")[[1]]))
  v2 <- as.integer(rev(strsplit(s2, "")[[1]]))
  n <- max(length(v1), length(v2))
  res <- c(v1, rep(0, n - length(v1))) + c(v2, rep(0, n - length(v2)))
  carry <- 0
  for (i in seq_along(res)) {
    val <- res[i] + carry
    res[i] <- val %% 10
    carry <- val %/% 10
  }
  if (carry > 0) res <- c(res, carry)
  paste(rev(res), collapse = "")
}

if (file.exists("input.txt")) {
  total <- "0"
  for (line in readLines("input.txt", warn = FALSE)) {
    chars <- strsplit(gsub("[^0-9]", "", line), "")[[1]]
    if (length(chars) < 12) next
    
    rem <- length(chars) - 12
    stack <- character(length(chars))
    top <- 0
    for (char in chars) {
      while (rem > 0 && top > 0 && char > stack[top]) {
        top <- top - 1
        rem <- rem - 1
      }
      top <- top + 1
      stack[top] <- char
    }
    total <- add_big(total, paste(stack[1:12], collapse = ""))
  }
  cat(total, "\n", sep = "")
}
