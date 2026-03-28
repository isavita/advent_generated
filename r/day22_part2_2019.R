options(scipen = 999)

mul_m <- function(a, b, m) {
  a <- a %% m
  b <- b %% m
  res <- 0
  while (b > 0) {
    if (b %% 2 == 1) res <- (res + a) %% m
    a <- (a + a) %% m
    b <- floor(b / 2)
  }
  res
}

pow_m <- function(a, k, m) {
  res <- 1
  a <- a %% m
  while (k > 0) {
    if (k %% 2 == 1) res <- mul_m(res, a, m)
    a <- mul_m(a, a, m)
    k <- floor(k / 2)
  }
  res
}

inv_m <- function(a, m) pow_m(a, m - 2, m)

get_t <- function(lines, m) {
  a <- 1
  b <- 0
  for (l in lines) {
    if (l == "deal into new stack") {
      a <- (-a) %% m
      b <- (-b - 1) %% m
    } else {
      n <- as.numeric(gsub(".* ", "", l))
      if (grepl("cut", l)) {
        b <- (b - n) %% m
      } else {
        a <- mul_m(a, n, m)
        b <- mul_m(b, n, m)
      }
    }
  }
  list(a = a, b = b)
}

lines <- readLines("input.txt")
lines <- lines[lines != ""]

m1 <- 10007
t1 <- get_t(lines, m1)
ans1 <- (mul_m(t1$a, 2019, m1) + t1$b) %% m1
cat("Part 1:", sprintf("%.0f", ans1), "\n")

m2 <- 119315717514047
k2 <- 101741582076661
t2 <- get_t(lines, m2)
ak <- pow_m(t2$a, k2, m2)
if (t2$a == 1) {
  bk <- mul_m(t2$b, k2, m2)
} else {
  bk <- mul_m(t2$b, mul_m(ak - 1, inv_m(t2$a - 1, m2), m2), m2)
}
ans2 <- mul_m(2020 - bk, inv_m(ak, m2), m2)
cat("Part 2:", sprintf("%.0f", ans2), "\n")