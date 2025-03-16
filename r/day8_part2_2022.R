
main <- function() {
    lines <- readLines("input.txt")
    grid <- list()
    for (y in seq_along(lines)) {
        line <- lines[[y]]
        for (x in seq_len(nchar(line))) {
            b <- as.integer(substr(line, x, x))
            grid[[paste(x - 1, y - 1, sep = ",")]] <- b
        }
    }

    neighbors4 <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), ncol = 2, byrow = TRUE)
    max_score <- 0

    for (p in names(grid)) {
        px <- as.integer(strsplit(p, ",")[[1]][1])
        py <- as.integer(strsplit(p, ",")[[1]][2])
        score <- 1
        for (i in 1:nrow(neighbors4)) {
            n <- neighbors4[i, ]
            next_pos <- c(px, py)
            view <- 0
            repeat {
                next_pos <- next_pos + n
                next_pos_str <- paste(next_pos[1], next_pos[2], sep = ",")
                if (next_pos_str %in% names(grid)) {
                    view <- view + 1
                    if (grid[[next_pos_str]] >= grid[[p]]) {
                      score <- score * view
                      break
                    }
                } else {
                    score <- score * view
                    break
                }
            }
        }
        max_score <- max(max_score, score)
    }
    cat(max_score)
}

main()
