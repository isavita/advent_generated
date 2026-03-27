
options(scipen = 999)

gcd <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
}

lcm <- function(a, b) {
  (a / gcd(a, b)) * b
}

find_cycle <- function(p) {
  p0 <- p
  v <- rep(0, length(p))
  s <- 0
  repeat {
    s <- s + 1
    for (i in 1:length(p)) {
      v[i] <- v[i] + sum(sign(p - p[i]))
    }
    p <- p + v
    if (all(v == 0) && all(p == p0)) return(s)
  }
}

lines <- readLines("input.txt", warn = FALSE)
nums <- as.numeric(unlist(regmatches(lines, gregexpr("-?\\d+", lines))))
moons <- matrix(nums, ncol = 3, byrow = TRUE)

cx <- find_cycle(moons[, 1])
cy <- find_cycle(moons[, 2])
cz <- find_cycle(moons[, 3])

ans <- lcm(lcm(cx, cy), cz)
cat(ans, "\n")
