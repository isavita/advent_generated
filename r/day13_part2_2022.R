
compare <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) return(sign(a - b))
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

lines <- readLines("input.txt", warn = FALSE)
lines <- lines[trimws(lines) != ""]

packets <- lapply(lines, function(l) {
  s <- gsub("[", "list(", l, fixed = TRUE)
  s <- gsub("]", ")", s, fixed = TRUE)
  eval(parse(text = s))
})

d1 <- list(list(2))
d2 <- list(list(6))

p1 <- 1
p2 <- 2

for (p in packets) {
  c1 <- compare(p, d1)
  if (c1 == -1) {
    p1 <- p1 + 1
    p2 <- p2 + 1
  } else if (compare(p, d2) == -1) {
    p2 <- p2 + 1
  }
}

cat(p1 * p2, "\n")
