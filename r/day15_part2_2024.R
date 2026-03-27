options(scipen = 999)

solve_task <- function(m_lines, moves) {
  m <- do.call(rbind, strsplit(m_lines, ""))
  nr <- nrow(m)
  nc <- ncol(m)
  curr <- which(m == "@", arr.ind = TRUE)[1, ]
  
  dirs <- list('^' = c(-1, 0), 'v' = c(1, 0), '<' = c(0, -1), '>' = c(0, 1))
  
  for (mv in moves) {
    d <- dirs[[mv]]
    to_move <- list(curr)
    visited <- matrix(FALSE, nr, nc)
    visited[curr[1], curr[2]] <- TRUE
    
    blocked <- FALSE
    idx <- 1
    while (idx <= length(to_move)) {
      p <- to_move[[idx]]
      idx <- idx + 1
      np <- p + d
      
      if (np[1] < 1 || np[1] > nr || np[2] < 1 || np[2] > nc) {
        blocked <- TRUE
        break
      }
      
      char <- m[np[1], np[2]]
      if (char == "#") {
        blocked <- TRUE
        break
      }
      if (char == ".") next
      
      if (!visited[np[1], np[2]]) {
        visited[np[1], np[2]] <- TRUE
        to_move[[length(to_move) + 1]] <- np
        
        if (d[1] != 0) {
          if (char == "[") {
            other <- np + c(0, 1)
            if (!visited[other[1], other[2]]) {
              visited[other[1], other[2]] <- TRUE
              to_move[[length(to_move) + 1]] <- other
            }
          } else if (char == "]") {
            other <- np + c(0, -1)
            if (!visited[other[1], other[2]]) {
              visited[other[1], other[2]] <- TRUE
              to_move[[length(to_move) + 1]] <- other
            }
          }
        }
      }
    }
    
    if (!blocked) {
      coords <- do.call(rbind, to_move)
      vals <- m[coords]
      m[coords] <- "."
      coords[, 1] <- coords[, 1] + d[1]
      coords[, 2] <- coords[, 2] + d[2]
      m[coords] <- vals
      curr <- curr + d
    }
  }
  
  boxes <- which(m == "O" | m == "[", arr.ind = TRUE)
  if (nrow(boxes) == 0) return(0)
  return(sum((boxes[, 1] - 1) * 100 + (boxes[, 2] - 1)))
}

scale_map <- function(lines) {
  sapply(lines, function(line) {
    chars <- strsplit(line, "")[[1]]
    new_chars <- sapply(chars, function(c) {
      if (c == "#") "##"
      else if (c == "O") "[]"
      else if (c == ".") ".."
      else if (c == "@") "@."
      else paste0(c, c)
    })
    paste(new_chars, collapse = "")
  }, USE.NAMES = FALSE)
}

input <- readLines("input.txt", warn = FALSE)
empty_line <- which(input == "")[1]
map_lines <- input[1:(empty_line - 1)]
moves <- unlist(strsplit(paste(input[(empty_line + 1):length(input)], collapse = ""), ""))
moves <- moves[moves %in% c('^', 'v', '<', '>')]

cat(solve_task(map_lines, moves), "\n")
cat(solve_task(scale_map(map_lines), moves), "\n")