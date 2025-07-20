
main <- function() {
  txt <- readLines("input.txt")
  blank <- which(txt == "")[1]
  map_lines <- txt[seq_len(blank - 1)]
  move_lines <- txt[-seq_len(blank)]
  moves <- paste(move_lines, collapse = "")

  grid <- do.call(rbind, strsplit(map_lines, ""))
  rows <- nrow(grid)
  cols <- ncol(grid)

  pos <- which(grid == "@", arr.ind = TRUE)
  robot_r <- pos[1, 1]
  robot_c <- pos[1, 2]
  grid[robot_r, robot_c] <- "."

  push_boxes <- function(r, c, dr, dc) {
    nr <- r + dr
    nc <- c + dc
    if (nr < 1 || nr > rows || nc < 1 || nc > cols) return(FALSE)
    if (grid[nr, nc] == "#") return(FALSE)
    if (grid[nr, nc] == "O") {
      if (!push_boxes(nr, nc, dr, dc)) return(FALSE)
    }
    if (grid[nr, nc] == ".") {
      grid[nr, nc] <<- "O"
      grid[r, c] <<- "."
      return(TRUE)
    }
    FALSE
  }

  for (m in strsplit(moves, "")[[1]]) {
    dr <- switch(m, "^" = -1, "v" = 1, 0)
    dc <- switch(m, "<" = -1, ">" = 1, 0)
    nr <- robot_r + dr
    nc <- robot_c + dc
    if (nr < 1 || nr > rows || nc < 1 || nc > cols) next
    cell <- grid[nr, nc]
    if (cell == "#") next
    if (cell == "O") {
      if (!push_boxes(nr, nc, dr, dc)) next
    }
    robot_r <- nr
    robot_c <- nc
  }

  boxes <- which(grid == "O", arr.ind = TRUE)
  total <- sum((boxes[, 1] - 1) * 100 + (boxes[, 2] - 1))
  cat(total, "\n")
}

main()
