
lines <- readLines("input.txt", n = 3)
h <- lapply(strsplit(gsub("@", ",", lines), ","), as.numeric)

p0 <- h[[1]][1:3]; v0 <- h[[1]][4:6]
p1 <- h[[2]][1:3]; v1 <- h[[2]][4:6]
p2 <- h[[3]][1:3]; v2 <- h[[3]][4:6]

cross <- function(a, b) {
  c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
}

r1p <- p1 - p0
r1v <- v1 - v0
r2p <- p2 - p0
r2v <- v2 - v0

pl1 <- cross(r1p, r1v)
pl2 <- cross(r2p, r2v)

t1 <- -sum(r1p * pl2) / sum(r1v * pl2)
t2 <- -sum(r2p * pl1) / sum(r2v * pl1)

rock1 <- p1 + v1 * t1
rock2 <- p2 + v2 * t2

v_rock <- (rock2 - rock1) / (t2 - t1)
p_rock <- rock1 - v_rock * t1

cat(sprintf("%.0f\n", sum(p_rock)))
