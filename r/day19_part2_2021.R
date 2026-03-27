rots_data <- c(
    1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, -1, 0, 1, 0,
    1, 0, 0, 0, -1, 0, 0, 0, -1, 1, 0, 0, 0, 0, 1, 0, -1, 0,
    0, -1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0,
    0, 1, 0, 1, 0, 0, 0, 0, -1, 0, 0, -1, 1, 0, 0, 0, -1, 0,
    -1, 0, 0, 0, -1, 0, 0, 0, 1, -1, 0, 0, 0, 0, -1, 0, -1, 0,
    -1, 0, 0, 0, 1, 0, 0, 0, -1, -1, 0, 0, 0, 0, 1, 0, 1, 0,
    0, 1, 0, -1, 0, 0, 0, 0, 1, 0, 0, 1, -1, 0, 0, 0, -1, 0,
    0, -1, 0, -1, 0, 0, 0, 0, -1, 0, 0, -1, -1, 0, 0, 0, 1, 0,
    0, 0, -1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0,
    0, 0, 1, 0, -1, 0, 1, 0, 0, 0, -1, 0, 0, 0, -1, 1, 0, 0,
    0, 0, -1, 0, -1, 0, -1, 0, 0, 0, -1, 0, 0, 0, 1, -1, 0, 0,
    0, 0, 1, 0, 1, 0, -1, 0, 0, 0, 1, 0, 0, 0, -1, -1, 0, 0
)

rotation_matrices <- lapply(seq(1, length(rots_data), by = 9), function(i) {
    matrix(rots_data[i:(i + 8)], 3, 3, byrow = TRUE)
})

find_match <- function(G, L_rots) {
    for (L in L_rots) {
        G_rep <- G[rep(1:nrow(G), each = nrow(L)), ]
        L_rep <- L[rep(1:nrow(L), times = nrow(G)), ]
        ds <- G_rep - L_rep
        keys <- paste(ds[, 1], ds[, 2], ds[, 3])
        s_keys <- sort.int(keys)
        runs <- rle(s_keys)
        if (any(runs$lengths >= 12)) {
            delta_str <- runs$values[which(runs$lengths >= 12)[1]]
            idx <- match(delta_str, keys)
            return(list(delta = ds[idx, ], rotated_L = L))
        }
    }
    NULL
}

lines <- readLines("input.txt")
scanners <- list()
for (line in lines) {
    if (grepl("--- scanner", line)) {
        scanners[[length(scanners) + 1]] <- matrix(nrow = 0, ncol = 3)
    } else if (nzchar(line) && grepl(",", line)) {
        scanners[[length(scanners)]] <- rbind(scanners[[length(scanners)]], as.numeric(strsplit(line, ",")[[1]]))
    }
}

all_rotated_scanners <- lapply(scanners, function(s) {
    lapply(rotation_matrices, function(m) s %*% t(m))
})

scanner_beacons <- vector("list", length(scanners))
scanner_beacons[[1]] <- scanners[[1]]
scanner_pos <- matrix(0, length(scanners), 3)
aligned <- rep(FALSE, length(scanners))
aligned[1] <- TRUE
queue <- 1

while (length(queue) > 0) {
    ref_idx <- queue[1]
    queue <- queue[-1]
    unaligned <- which(!aligned)
    for (target_idx in unaligned) {
        match <- find_match(scanner_beacons[[ref_idx]], all_rotated_scanners[[target_idx]])
        if (!is.null(match)) {
            scanner_pos[target_idx, ] <- match$delta
            scanner_beacons[[target_idx]] <- sweep(match$rotated_L, 2, match$delta, "+")
            aligned[target_idx] <- TRUE
            queue <- c(queue, target_idx)
        }
    }
}

max_dist <- max(dist(scanner_pos, method = "manhattan"))
cat(as.integer(max_dist), "\n")