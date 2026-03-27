
big_add <- function(a, b) {
  a <- as.integer(strsplit(a, "")[[1]])
  b <- as.integer(strsplit(b, "")[[1]])
  n <- max(length(a), length(b))
  res <- integer(n + 1)
  a <- c(integer(n - length(a)), a)
  b <- c(integer(n - length(b)), b)
  carry <- 0
  for (i in n:1) {
    s <- a[i] + b[i] + carry
    res[i + 1] <- s %% 10
    carry <- s %/% 10
  }
  res[1] <- carry
  while (length(res) > 1 && res[1] == 0) res <- res[-1]
  paste(res, collapse = "")
}

big_mul <- function(a, b) {
  if (a == "0" || b == "0") return("0")
  a <- as.integer(strsplit(a, "")[[1]])
  b <- as.integer(strsplit(b, "")[[1]])
  res <- numeric(length(a) + length(b))
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      res[i + j] <- res[i + j] + a[i] * b[j]
    }
  }
  for (i in length(res):2) {
    res[i - 1] <- res[i - 1] + res[i] %/% 10
    res[i] <- res[i] %% 10
  }
  while (length(res) > 1 && res[1] == 0) res <- res[-1]
  paste(as.integer(res), collapse = "")
}

process_block <- function(sc, ec, lines) {
  tokens <- character(0)
  for (line in lines) {
    token <- trimws(substr(line, sc, ec))
    if (token != "") tokens <- c(tokens, token)
  }
  if (length(tokens) == 0) return("0")
  op <- if ("+" %in% tokens) 1 else if ("*" %in% tokens) 2 else 0
  nums <- tokens[!tokens %in% c("+", "*")]
  if (op == 1) {
    acc <- "0"
    for (n in nums) acc <- big_add(acc, n)
    return(acc)
  } else if (op == 2) {
    acc <- "1"
    for (n in nums) acc <- big_mul(acc, n)
    return(acc)
  } else if (length(nums) > 0) {
    return(nums[1])
  }
  "0"
}

lines <- readLines("input.txt", warn = FALSE)
if (length(lines) == 0) {
  cat("Grand total: 0\n")
} else {
  maxw <- max(nchar(lines))
  lines <- sprintf(paste0("%-", maxw, "s"), lines)
  is_sep <- function(x) all(substr(lines, x, x) == " ")
  grand <- "0"
  inb <- FALSE
  sc <- 1
  for (x in 1:maxw) {
    if (!is_sep(x)) {
      if (!inb) { inb <- TRUE; sc <- x }
    } else {
      if (inb) {
        grand <- big_add(grand, process_block(sc, x - 1, lines))
        inb <- FALSE
      }
    }
  }
  if (inb) grand <- big_add(grand, process_block(sc, maxw, lines))
  cat(sprintf("Grand total: %s\n", grand))
}
