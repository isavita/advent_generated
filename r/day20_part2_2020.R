
main <- function() {
  txt <- readLines("input.txt")
  tiles <- list()
  id <- NULL
  rows <- list()
  for (l in txt) {
    if (grepl("^Tile", l)) {
      if (!is.null(id)) {
        tiles[[as.character(id)]] <- do.call(rbind, rows)
      }
      id <- as.integer(gsub("^Tile (\\d+):", "\\1", l))
      rows <- list()
    } else if (nchar(trimws(l)) > 0) {
      rows[[length(rows)+1]] <- strsplit(l, "")[[1]]
    }
  }
  if (!is.null(id)) {
    tiles[[as.character(id)]] <- do.call(rbind, rows)
  }

  n <- sqrt(length(tiles))
  if (n != floor(n)) stop("non-square tile count")
  n <- as.integer(n)
  k <- nrow(tiles[[1]])
  m <- k - 2
  img_dim <- n * m

  orient <- function(g) {
    g0 <- g
    g90 <- t(g0[nrow(g0):1, ])
    g180 <- t(g90[nrow(g90):1, ])
    g270 <- t(g180[nrow(g180):1, ])
    g0f <- g0[, ncol(g0):1]
    g90f <- t(g0f[nrow(g0f):1, ])
    g180f <- t(g90f[nrow(g90f):1, ])
    g270f <- t(g180f[nrow(g180f):1, ])
    list(g0, g90, g180, g270, g0f, g90f, g180f, g270f)
  }

  tiles_orient <- lapply(tiles, orient)
  ids <- names(tiles_orient)

  used <- logical(length(tiles))
  grid <- matrix(list(), n, n)

  backtrack <- function(r, c) {
    if (r > n) return(TRUE)
    nr <- ifelse(c == n, r + 1, r)
    nc <- ifelse(c == n, 1, c + 1)
    for (i in seq_along(tiles_orient)) {
      if (used[i]) next
      for (o_idx in seq_along(tiles_orient[[i]])) {
        cand <- tiles_orient[[i]][[o_idx]]
        ok <- TRUE
        if (r > 1) {
          above <- grid[r-1, c][[1]]
          if (!all(cand[1, ] == above[k, ])) ok <- FALSE
        }
        if (ok && c > 1) {
          left <- grid[r, c-1][[1]]
          if (!all(cand[, 1] == left[, k])) ok <- FALSE
        }
        if (ok) {
          used[i] <<- TRUE
          grid[r, c] <<- list(cand)
          if (backtrack(nr, nc)) return(TRUE)
          used[i] <<- FALSE
        }
      }
    }
    FALSE
  }

  if (!backtrack(1, 1)) stop("assembly failed")

  img <- matrix(NA, img_dim, img_dim)
  for (i in 1:n) {
    for (j in 1:n) {
      tile <- grid[i, j][[1]][2:(k-1), 2:(k-1)]
      img[((i-1)*m + 1):(i*m), ((j-1)*m + 1):(j*m)] <- tile
    }
  }

  monster <- matrix(c(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ), 3, byrow = TRUE)
  mh <- nrow(monster)
  mw <- nchar(monster[1])
  pat <- do.call(rbind, strsplit(monster, "")) == "#"
  coords <- which(pat, arr.ind = TRUE)
  coords[, 1] <- coords[, 1] - 1
  coords[, 2] <- coords[, 2] - 1

  img_orient <- orient(img)
  roughness <- NULL
  for (g in img_orient) {
    found <- FALSE
    marked <- matrix(FALSE, nrow(g), ncol(g))
    for (r in 1:(nrow(g) - mh + 1)) {
      for (c in 1:(ncol(g) - mw + 1)) {
        hit <- TRUE
        for (k in seq_len(nrow(coords))) {
          dr <- coords[k, 1]
          dc <- coords[k, 2]
          if (g[r + dr, c + dc] != "#") {
            hit <- FALSE
            break
          }
        }
        if (hit) {
          found <- TRUE
          for (k in seq_len(nrow(coords))) {
            dr <- coords[k, 1]
            dc <- coords[k, 2]
            marked[r + dr, c + dc] <- TRUE
          }
        }
      }
    }
    if (found) {
      roughness <- sum(g == "#") - sum(marked)
      break
    }
  }

  cat(roughness, "\n")
}

main()
