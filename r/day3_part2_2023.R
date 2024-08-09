Neighbors8 <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0), c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))

valid <- function(p, grid) {
  for (x in p$xmin:p$xmax) {
    for (n in Neighbors8) {
      c <- grid[[paste(x + n[1], p$y + n[2], sep = ",")]]
      if (!is.null(c) && c != '.' && !(c >= '0' && c <= '9')) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

input <- readLines("input.txt")
grid <- list()
parts <- list()
curr <- NULL

for (y in seq_along(input)) {
  line <- strsplit(input[y], "")[[1]]
  if (!is.null(curr)) {
    parts[[length(parts) + 1]] <- curr
    curr <- NULL
  }
  for (x in seq_along(line)) {
    c <- line[x]
    grid[[paste(x - 1, y - 1, sep = ",")]] <- c
    if (c >= '0' && c <= '9') {
      if (is.null(curr)) {
        curr <- list(xmin = x - 1, xmax = x - 1, y = y - 1, n = as.integer(c))
      } else {
        curr$n <- curr$n * 10 + as.integer(c)
        curr$xmax <- x - 1
      }
    } else if (!is.null(curr)) {
      parts[[length(parts) + 1]] <- curr
      curr <- NULL
    }
  }
}

partsGrid <- list()
for (i in seq_along(parts)) {
  p <- parts[[i]]
  for (x in p$xmin:p$xmax) {
    partsGrid[[paste(x, p$y, sep = ",")]] <- i - 1
  }
}

sum <- 0
for (p in names(grid)) {
  if (grid[[p]] == '*') {
    neighborParts <- unique(unlist(lapply(Neighbors8, function(n) partsGrid[[paste(as.integer(strsplit(p, ",")[[1]][1]) + n[1], 
                                                                                                       as.integer(strsplit(p, ",")[[1]][2]) + n[2], sep = ",")]])))
    if (length(neighborParts) == 2) {
      prod <- 1
      for (i in neighborParts) {
        prod <- prod * parts[[i + 1]]$n
      }
      sum <- sum + prod
    }
  }
}
print(sum)