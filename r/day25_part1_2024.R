
parse_lock <- function(b) {
    h <- integer(5)
    for (c in 1:5) {
        cnt <- 0
        for (r in 2:7) {
            if (b[r, c] == '#') {
                cnt <- cnt + 1
            } else {
                break
            }
        }
        h[c] <- cnt
    }
    h
}

parse_key <- function(b) {
    h <- integer(5)
    for (c in 1:5) {
        cnt <- 0
        for (r in 6:1) {
            if (b[r, c] == '#') {
                cnt <- cnt + 1
            } else {
                break
            }
        }
        h[c] <- cnt
    }
    h
}

fits <- function(lock, key) {
    all(lock + key <= 5)
}

solve <- function() {
    raw <- readLines("input.txt")
    raw <- raw[raw != ""]
    
    if (length(raw) %% 7 != 0) {
        cat(0, "\n")
        return()
    }

    locks <- list()
    keys <- list()
    
    for (i in seq(1, length(raw), by = 7)) {
      block <- raw[i:(i + 6)]
      block_matrix <- matrix(unlist(strsplit(block, "")), nrow = 7, byrow = TRUE)[,1:5]
      
      if (any(nchar(block) < 5)) {
        next
      }

      if (all(block_matrix[1, ] == '#')) {
          locks <- c(locks, list(parse_lock(block_matrix)))
      } else {
          keys <- c(keys, list(parse_key(block_matrix)))
      }
    }

    count <- 0
    for (lock in locks) {
        for (key in keys) {
            if (fits(unlist(lock), unlist(key))) {
                count <- count + 1
            }
        }
    }
    cat(count, "\n")
}

main <- function() {
    solve()
}

main()
