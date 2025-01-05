
solve <- function(input) {
  parts <- strsplit(input, "\n\n")[[1]]
  nums <- as.integer(strsplit(parts[1], ",")[[1]])
  boards <- lapply(parts[-1], function(board_str) {
    rows <- strsplit(trimws(board_str), "\n")[[1]]
    board <- t(sapply(rows, function(row) {
      as.integer(strsplit(trimws(gsub(" +", " ", row)), " ")[[1]])
    }))
    list(board = board, picked = matrix(FALSE, nrow = nrow(board), ncol = ncol(board)))
  })
  
  last_winning_score <- -1
  already_won <- rep(FALSE, length(boards))
  
  for (n in nums) {
    for (bi in seq_along(boards)) {
      if (already_won[bi]) next
      
      board <- boards[[bi]]
      
      for (r in 1:nrow(board$board)) {
        for (c in 1:ncol(board$board)) {
          if (board$board[r, c] == n) {
            board$picked[r, c] <- TRUE
          }
        }
      }
      
      did_win <- FALSE
      for (i in 1:nrow(board$board)) {
        if (all(board$picked[i, ]) || all(board$picked[, i])) {
          did_win <- TRUE
          break
        }
      }
      
      if (did_win) {
        score <- sum(board$board[!board$picked])
        last_winning_score <- score * n
        already_won[bi] <- TRUE
      }
      boards[[bi]] <- board
    }
  }
  
  last_winning_score
}

input <- paste(readLines("input.txt"), collapse = "\n")
result <- solve(input)
cat(result, "\n")
