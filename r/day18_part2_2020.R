
evaluate <- function(line, part) {
  tokens <- unlist(regmatches(line, gregexpr("[0-9]+|\\+|\\*|\\(|\\)", line)))
  nums <- numeric()
  ops <- character()
  
  prec <- function(o) {
    if (o == "(") return(0)
    if (part == 1) return(1)
    if (o == "+") return(2)
    return(1)
  }
  
  apply_op <- function() {
    o <- tail(ops, 1); ops <<- head(ops, -1)
    r <- tail(nums, 1); nums <<- head(nums, -1)
    l <- tail(nums, 1); nums <<- head(nums, -1)
    nums <<- c(nums, if (o == "+") l + r else l * r)
  }
  
  for (t in tokens) {
    if (t == "(") {
      ops <- c(ops, "(")
    } else if (t == ")") {
      while (tail(ops, 1) != "(") apply_op()
      ops <- head(ops, -1)
    } else if (t %in% c("+", "*")) {
      while (length(ops) > 0 && prec(tail(ops, 1)) >= prec(t)) apply_op()
      ops <- c(ops, t)
    } else {
      nums <- c(nums, as.numeric(t))
    }
  }
  while (length(ops) > 0) apply_op()
  return(nums)
}

lines <- readLines("input.txt", warn = FALSE)
lines <- lines[nchar(lines) > 0]

ans1 <- sum(sapply(lines, evaluate, part = 1))
ans2 <- sum(sapply(lines, evaluate, part = 2))

cat(sprintf("%.0f\n%.0f\n", ans1, ans2))
