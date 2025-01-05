
grid <- readLines("input.txt")
grid <- grid[grid != ""]
rows <- length(grid)
cols <- nchar(grid[1])
grid_matrix <- matrix(unlist(strsplit(grid, "")), nrow = rows, byrow = TRUE)
word <- "XMAS"
word_len <- nchar(word)
count <- 0
directions <- list(c(0, 1), c(1, 0), c(1, 1), c(-1, 1), c(0, -1), c(-1, 0), c(-1, -1), c(1, -1))
for (i in 1:rows) {
    for (j in 1:cols) {
        for (d in directions) {
            dx <- d[1]
            dy <- d[2]
            valid <- TRUE
            for (k in 1:word_len) {
                new_x <- i + (dx * (k - 1))
                new_y <- j + (dy * (k - 1))
                if (new_x < 1 || new_y < 1 || new_x > rows || new_y > cols || grid_matrix[new_x, new_y] != substr(word, k, k)) {
                    valid <- FALSE
                    break
                }
            }
            if (valid) {
                count <- count + 1
            }
        }
    }
}
cat(sprintf("XMAS appears %d times in the word search\n", count))
