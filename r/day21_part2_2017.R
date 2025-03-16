
rotate <- function(input_str) {
  parts <- strsplit(input_str, "/")[[1]]
  size <- length(parts)
  new_parts <- character(size)
  for (x in 1:size) {
    new_row <- ""
    for (y in size:1) {
      new_row <- paste0(new_row, substr(parts[y], x, x))
    }
    new_parts[x] <- new_row
  }
  paste(new_parts, collapse = "/")
}

flip <- function(input_str) {
  parts <- strsplit(input_str, "/")[[1]]
  paste(sapply(parts, function(x) paste(rev(strsplit(x, "")[[1]]), collapse = "")), collapse = "/")
}

enhance <- function(input_str, rules, memo) {
  if (!is.null(memo[[input_str]])) {
    return(list(output = memo[[input_str]], memo = memo))
  }
  
  original <- input_str
  
  temp_str <- input_str
    for (i in 1:4) {
      if (!is.null(rules[[temp_str]])) {
        memo[[original]] <- rules[[temp_str]]
        return(list(output = rules[[temp_str]], memo = memo))
      }
        temp_str <- rotate(temp_str)
    }
    
    temp_str <- flip(input_str)
    for (i in 1:4){
        if (!is.null(rules[[temp_str]])) {
          memo[[original]] <- rules[[temp_str]]
          return(list(output = rules[[temp_str]], memo = memo))
        }
      temp_str <- rotate(temp_str)
    }

  return(list(output = "", memo = memo))
}

main <- function() {
  rules <- new.env()
  memo <- new.env()
  
  con <- file("input.txt", "r")
  while (length(line <- readLines(con, n = 1)) > 0) {
    parts <- strsplit(line, " => ")[[1]]
    rules[[parts[1]]] <- parts[2]
  }
  close(con)

  grid <- c(".#.", "..#", "###")

  for (i in 1:18) {
    if (length(grid) %% 2 == 0) {
      sub_size <- 2
      new_size <- length(grid) %/% 2 * 3
    } else {
      sub_size <- 3
      new_size <- length(grid) %/% 3 * 4
    }

    new_grid <- rep("", new_size)

    for (y in seq(1, length(grid), by = sub_size)) {
      for (x in seq(1, length(grid), by = sub_size)) {
        square <- character(sub_size)
        for (dy in 1:sub_size) {
          square[dy] <- substr(grid[y + dy - 1], x, x + sub_size - 1)
        }
        enhanced <- enhance(paste(square, collapse = "/"), rules, memo)
        new_square <- enhanced$output
        memo <- enhanced$memo
        
        rows <- strsplit(new_square, "/")[[1]]
        for (dy in seq_along(rows)) {
          new_grid[(y %/% sub_size) * (sub_size + 1) + dy] <- paste0(new_grid[(y %/% sub_size) * (sub_size + 1) + dy], rows[dy])
        }
      }
    }
    grid <- new_grid
  }

  count <- 0
  for (row in grid) {
    count <- count + sum(strsplit(row, "")[[1]] == "#")
  }
  cat(count, "\n")
}

main()
