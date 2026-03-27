
options(scipen = 999)
prog <- scan("input.txt", sep = ",", quiet = TRUE)
mem <- numeric(1e6)
mem[1:length(prog)] <- prog
mem[1] <- 2
ip <- 1
rb <- 0
bx <- px <- sc <- st <- tx <- ty <- 0
while (TRUE) {
  ins <- mem[ip]
  op <- ins %% 100
  m <- (ins %/% c(100, 1000, 10000)) %% 10
  if (op == 99) break
  v <- function(n) {
    idx <- if (m[n] == 0) mem[ip+n]+1 else if (m[n] == 1) ip+n else rb+mem[ip+n]+1
    mem[idx]
  }
  a <- function(n) if (m[n] == 0) mem[ip+n]+1 else rb+mem[ip+n]+1
  if (op == 1) {
    mem[a(3)] <- v(1) + v(2)
    ip <- ip + 4
  } else if (op == 2) {
    mem[a(3)] <- v(1) * v(2)
    ip <- ip + 4
  } else if (op == 3) {
    mem[a(1)] <- sign(bx - px)
    ip <- ip + 2
  } else if (op == 4) {
    out <- v(1)
    if (st == 0) {
      tx <- out
      st <- 1
    } else if (st == 1) {
      ty <- out
      st <- 2
    } else {
      if (tx == -1 && ty == 0) sc <- out else if (out == 3) px <- tx else if (out == 4) bx <- tx
      st <- 0
    }
    ip <- ip + 2
  } else if (op == 5) {
    if (v(1) != 0) ip <- v(2) + 1 else ip <- ip + 3
  } else if (op == 6) {
    if (v(1) == 0) ip <- v(2) + 1 else ip <- ip + 3
  } else if (op == 7) {
    mem[a(3)] <- as.numeric(v(1) < v(2))
    ip <- ip + 4
  } else if (op == 8) {
    mem[a(3)] <- as.numeric(v(1) == v(2))
    ip <- ip + 4
  } else if (op == 9) {
    rb <- rb + v(1)
    ip <- ip + 2
  }
}
cat(sc, "\n")
