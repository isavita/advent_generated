
xor_num <- function(a, b) {
  res <- 0
  bit <- 1
  while (a > 0 || b > 0) {
    if ((a %% 2) != (b %% 2)) res <- res + bit
    a <- floor(a / 2)
    b <- floor(b / 2)
    bit <- bit * 2
  }
  res
}

simulate <- function(a, b, c, prog) {
  ip <- 0
  n <- length(prog)
  while (ip < n) {
    cmd <- prog[ip + 1]
    op <- prog[ip + 2]
    cb <- if (op <= 3) op else if (op == 4) a else if (op == 5) b else if (op == 6) c else 0
    if (cmd == 0) a <- floor(a / (2^cb))
    else if (cmd == 1) b <- xor_num(b, op)
    else if (cmd == 2) b <- cb %% 8
    else if (cmd == 3) {
      if (a != 0) { ip <- op; next }
    } else if (cmd == 4) b <- xor_num(b, c)
    else if (cmd == 5) return(cb %% 8)
    else if (cmd == 6) b <- floor(a / (2^cb))
    else if (cmd == 7) c <- floor(a / (2^cb))
    ip <- ip + 2
  }
  -1
}

lines <- readLines("input.txt", warn = FALSE)
ib <- as.numeric(sub(".*: ", "", lines[grep("Register B", lines)]))
ic <- as.numeric(sub(".*: ", "", lines[grep("Register C", lines)]))
pr <- as.numeric(unlist(strsplit(sub(".*: ", "", lines[grep("Program", lines)]), ",")))

find_a <- function(depth, score) {
  if (depth == length(pr)) return(score)
  res_list <- NULL
  for (i in 0:7) {
    ns <- score * 8 + i
    if (simulate(ns, ib, ic, pr) == pr[length(pr) - depth]) {
      res <- find_a(depth + 1, ns)
      if (!is.null(res)) res_list <- c(res_list, res)
    }
  }
  if (is.null(res_list)) return(NULL)
  min(res_list)
}

ans <- find_a(0, 0)
cat(format(ans, scientific = FALSE), "\n")
