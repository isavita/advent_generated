
main <- function() {
    lines <- readLines("input.txt")
    grid <- new.env(hash = TRUE)
    
    for (y in seq_along(lines)) {
        line <- lines[y]
        chars <- strsplit(line, "")[[1]]
        for (x in seq_along(chars)) {
            if (chars[x] == '#') {
                assign(paste(x, y, sep = ","), 2, envir = grid)
            }
        }
    }

    startX <- nchar(lines[1]) %/% 2 +1
    startY <- length(lines) %/% 2 +1

    dx <- c(0, 1, 0, -1)
    dy <- c(-1, 0, 1, 0)

    x <- startX
    y <- startY
    dir <- 1
    infectedCount <- 0

    for (i in 1:10000000) {
        pos <- paste(x, y, sep = ",")
        val <- tryCatch(
          get(pos, envir = grid),
          error = function(e) 0
        )

        if (val == 0) {
            dir <- (dir - 1 + 4 - 1) %% 4 + 1
            assign(pos, 1, envir = grid)
        } else if (val == 1) {
            assign(pos, 2, envir = grid)
            infectedCount <- infectedCount + 1
        } else if (val == 2) {
            dir <- (dir + 1 - 1) %% 4 + 1
            assign(pos, 3, envir = grid)
        } else if (val == 3) {
            dir <- (dir + 2 - 1) %% 4 + 1
            assign(pos, 0, envir = grid)
        }

        x <- x + dx[dir]
        y <- y + dy[dir]
    }

    cat(infectedCount, "\n")
}

main()
