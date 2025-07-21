
main <- function() {
  lines <- readLines("input.txt")
  k <- integer(); l <- integer(); m <- integer()
  for (i in seq_along(lines)) {
    idx <- (i - 1) %% 18
    if (idx == 4)  l <- c(l, as.integer(strsplit(lines[i], " +")[[1]][3]))
    if (idx == 5)  k <- c(k, as.integer(strsplit(lines[i], " +")[[1]][3]))
    if (idx == 15) m <- c(m, as.integer(strsplit(lines[i], " +")[[1]][3]))
  }

  constraints <- list(); stack <- integer()
  for (i in seq_along(l)) {
    if (l[i] == 1) {
      stack <- c(stack, i)
    } else {
      pop <- stack[length(stack)]
      stack <- stack[-length(stack)]
      constraints[[as.character(pop)]] <- c(i, m[pop] + k[i])
    }
  }

  max_vals <- rep(0, 14)
  for (i in seq_along(max_vals)) {
    if (is.null(constraints[[as.character(i)]])) next
    vmax <- 9
    while (vmax + constraints[[as.character(i)]][2] > 9) vmax <- vmax - 1
    max_vals[i] <- vmax
    max_vals[constraints[[as.character(i)]][1]] <- vmax + constraints[[as.character(i)]][2]
  }

  cat(paste(max_vals, collapse = ""), "\n")
}

main()
