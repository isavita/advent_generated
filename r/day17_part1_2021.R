
main <- function() {
  line <- readLines("input.txt")
  parts <- strsplit(line, ", ")[[1]]
  x_range <- as.integer(strsplit(substr(parts[1], 16, nchar(parts[1])), "\\.\\.")[[1]])
  y_range <- as.integer(strsplit(substr(parts[2], 3, nchar(parts[2])), "\\.\\.")[[1]])
  x_min <- x_range[1]
  x_max <- x_range[2]
  y_min <- y_range[1]
  y_max <- y_range[2]

  max_y <- -1e30
  for (x_vel in 0:x_max) {
    for (y_vel in y_min:1000) {
      x_pos <- 0
      y_pos <- 0
      cur_x_vel <- x_vel
      cur_y_vel <- y_vel
      highest_y <- y_pos
      while (x_pos <= x_max && y_pos >= y_min) {
        x_pos <- x_pos + cur_x_vel
        y_pos <- y_pos + cur_y_vel
        highest_y <- max(highest_y, y_pos)

        if (x_min <= x_pos && x_max >= x_pos && y_min <= y_pos && y_max >= y_pos) {
          max_y <- max(max_y, highest_y)
          break
        }

        if (cur_x_vel > 0) {
          cur_x_vel <- cur_x_vel - 1
        }
        cur_y_vel <- cur_y_vel - 1
      }
    }
  }
  cat(max_y, "\n")
}

main()
