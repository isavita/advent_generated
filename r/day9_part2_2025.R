pts <- tryCatch({
  as.matrix(read.table("input.txt", sep = ","))
}, error = function(e) matrix(0, 0, 2))

if (nrow(pts) == 0) {
  cat("Largest valid area: 0\n")
} else {
  n <- nrow(pts)
  ux <- sort(unique(pts[, 1]))
  uy <- sort(unique(pts[, 2]))
  nx <- length(ux); ny <- length(uy)
  W <- 2 * nx + 1; H <- 2 * ny + 1
  colW <- rep(1, W)
  if (nx > 0) colW[seq(2, 2 * nx, by = 2)] <- 1
  if (nx > 1) colW[seq(3, 2 * nx - 1, by = 2)] <- diff(ux) - 1
  rowH <- rep(1, H)
  if (ny > 0) rowH[seq(2, 2 * ny, by = 2)] <- 1
  if (ny > 1) rowH[seq(3, 2 * ny - 1, by = 2)] <- diff(uy) - 1
  grid <- matrix(0, H, W)
  xm <- match(pts[, 1], ux); ym <- match(pts[, 2], uy)
  for (i in 1:n) {
    p1 <- i; p2 <- if (i == n) 1 else i + 1
    gx1 <- 2 * xm[p1]; gy1 <- 2 * ym[p1]
    gx2 <- 2 * xm[p2]; gy2 <- 2 * ym[p2]
    if (gx1 == gx2) grid[min(gy1, gy2):max(gy1, gy2), gx1] <- 1
    else grid[gy1, min(gx1, gx2):max(gx1, gx2)] <- 1
  }
  qr <- integer(W * H); qc <- integer(W * H)
  qr[1] <- 1; qc[1] <- 1; grid[1, 1] <- 2; head <- 1; tail <- 2
  dr <- c(0, 0, 1, -1); dc <- c(1, -1, 0, 0)
  while (head < tail) {
    cr <- qr[head]; cc <- qc[head]; head <- head + 1
    for (k in 1:4) {
      nr <- cr + dr[k]; nc <- cc + dc[k]
      if (nr >= 1 && nr <= H && nc >= 1 && nc <= W && grid[nr, nc] == 0) {
        grid[nr, nc] <- 2; qr[tail] <- nr; qc[tail] <- nc; tail <- tail + 1
      }
    }
  }
  v <- outer(rowH, colW, "*"); v[grid == 2] <- 0
  P <- matrix(0, H, W)
  for (i in 1:H) P[i, ] <- cumsum(v[i, ])
  for (j in 1:W) P[, j] <- cumsum(P[, j])
  P_p <- matrix(0, H + 1, W + 1); P_p[2:(H + 1), 2:(W + 1)] <- P
  maxA <- 0; px <- pts[, 1]; py <- pts[, 2]; gxv <- 2 * xm; gyv <- 2 * ym
  for (i in 1:n) {
    axi <- px[i]; ayi <- py[i]; gxi <- gxv[i]; gyi <- gyv[i]
    for (j in i:n) {
      a <- (abs(axi - px[j]) + 1) * (abs(ayi - py[j]) + 1)
      if (a <= maxA) next
      r1 <- if (gyi < gyv[j]) gyi else gyv[j]; r2 <- if (gyi > gyv[j]) gyi else gyv[j]
      c1 <- if (gxi < gxv[j]) gxi else gxv[j]; c2 <- if (gxi > gxv[j]) gxi else gxv[j]
      val <- P_p[r2 + 1, c2 + 1] - P_p[r1, c2 + 1] - P_p[r2 + 1, c1] + P_p[r1, c1]
      if (abs(val - a) < 0.5) maxA <- a
    }
  }
  cat(sprintf("Largest valid area: %.0f\n", maxA))
}