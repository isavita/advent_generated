
compare <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) {
    return(sign(a - b))
  }
  if (is.numeric(a)) a <- list(a)
  if (is.numeric(b)) b <- list(b)
  
  na <- length(a)
  nb <- length(b)
  if (na > 0 && nb > 0) {
    for (i in seq_len(min(na, nb))) {
      res <- compare(a[[i]], b[[i]])
      if (res != 0) return(res)
    }
  }
  return(sign(na - nb))
}

input <- readLines("input.txt", warn = FALSE)
input <- input[input != ""]
input <- gsub("\\]", ")", gsub("\\[", "list(", input))

total_sum <- 0
if (length(input) >= 2) {
  for (i in seq(1, length(input), by = 2)) {
    p1 <- eval(parse(text = input[i]))
    p2 <- eval(parse(text = input[i + 1]))
    if (compare(p1, p2) == -1) {
      total_sum <- total_sum + (i + 1) / 2
    }
  }
}

cat(total_sum, "\n")
