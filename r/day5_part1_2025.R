lines <- trimws(readLines("input.txt", warn = FALSE))
sep <- which(lines == "")[1]
if (is.na(sep)) sep <- length(lines) + 1

r_txt <- lines[seq_len(sep - 1)]
r_txt <- r_txt[r_txt != ""]
lo <- hi <- numeric(0)

if (length(r_txt) > 0) {
  r_m <- matrix(as.numeric(unlist(strsplit(r_txt, "-"))), ncol = 2, byrow = TRUE)
  r_m <- r_m[order(r_m[, 1], r_m[, 2]), , drop = FALSE]
  lo <- r_m[, 1]; hi <- r_m[, 2]; k <- 1
  for (i in seq_along(lo)[-1]) {
    if (lo[i] <= hi[k]) hi[k] <- max(hi[k], hi[i])
    else { k <- k + 1; lo[k] <- lo[i]; hi[k] <- hi[i] }
  }
  lo <- lo[seq_len(k)]; hi <- hi[seq_len(k)]
}

ans <- 0
if (length(lo) > 0) {
  ids <- as.numeric(lines[seq_along(lines) > sep])
  ids <- ids[!is.na(ids)]
  if (length(ids) > 0) {
    idx <- findInterval(ids, lo)
    ans <- sum(idx > 0 & ids <= hi[idx])
  }
}

cat(sprintf("Number of fresh ingredients: %d\n", ans))