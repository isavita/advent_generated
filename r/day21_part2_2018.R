
f <- readLines("input.txt")
c1 <- as.numeric(strsplit(f[9], " ")[[1]][2])
c2 <- as.numeric(strsplit(f[13], " ")[[1]][3])
r5 <- 0
last <- 0
s <- new.env()
while (TRUE) {
  r4 <- bitwOr(r5, 65536)
  r5 <- c1
  while (TRUE) {
    r5 <- (((r5 + bitwAnd(r4, 255)) %% 16777216) * c2) %% 16777216
    if (r4 < 256) break
    r4 <- r4 %/% 256
  }
  k <- as.character(r5)
  if (exists(k, envir = s)) {
    cat(last, "\n")
    break
  }
  assign(k, TRUE, envir = s)
  last <- r5
}

