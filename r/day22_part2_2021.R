options(scipen = 999)
lines <- readLines("input.txt")
df <- utils::strcapture("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)", 
                        lines, 
                        data.frame(s="", x1=0, x2=0, y1=0, y2=0, z1=0, z2=0))
m <- as.matrix(df[, 2:7])
q <- matrix(nrow = 0, ncol = 7)
for (i in seq_len(nrow(df))) {
  x <- m[i, ]
  n <- matrix(nrow = 0, ncol = 7)
  if (nrow(q) > 0) {
    ix1 <- pmax(q[, 1], x[1]); ix2 <- pmin(q[, 2], x[2])
    iy1 <- pmax(q[, 3], x[3]); iy2 <- pmin(q[, 4], x[4])
    iz1 <- pmax(q[, 5], x[5]); iz2 <- pmin(q[, 6], x[6])
    v <- ix1 <= ix2 & iy1 <= iy2 & iz1 <= iz2
    if (any(v)) n <- cbind(ix1[v], ix2[v], iy1[v], iy2[v], iz1[v], iz2[v], -q[v, 7])
  }
  if (df$s[i] == "on") n <- rbind(n, c(x, 1))
  q <- rbind(q, n)
}
ans <- if (nrow(q) > 0) sum((q[, 2] - q[, 1] + 1) * (q[, 4] - q[, 3] + 1) * (q[, 6] - q[, 5] + 1) * q[, 7]) else 0
cat(sprintf("%.0f\n", ans))