
program <- as.numeric(strsplit(readLines("input.txt", warn = FALSE), ",")[[1]])
mem <- numeric(1e6)
mem[1:length(program)] <- program
ip <- 0
halted <- FALSE
grid <- new.env()
pos <- c(0, 0)
dir <- 0

run_intcode <- function(input_val) {
  outputs <- numeric(0)
  input_used <- FALSE
  while (TRUE) {
    op_full <- mem[ip + 1]
    op <- op_full %% 100
    m1 <- floor(op_full / 100) %% 10
    m2 <- floor(op_full / 1000) %% 10
    
    if (op == 99) {
      halted <<- TRUE
      return(outputs)
    }
    
    val <- function(idx, m) {
      p <- mem[ip + idx + 1]
      if (m == 0) return(mem[p + 1])
      return(p)
    }
    
    if (op == 1) {
      mem[mem[ip + 4] + 1] <<- val(1, m1) + val(2, m2)
      ip <<- ip + 4
    } else if (op == 2) {
      mem[mem[ip + 4] + 1] <<- val(1, m1) * val(2, m2)
      ip <<- ip + 4
    } else if (op == 3) {
      if (input_used) return(outputs)
      mem[mem[ip + 2] + 1] <<- input_val
      input_used <<- TRUE
      ip <<- ip + 2
    } else if (op == 4) {
      outputs <- c(outputs, val(1, m1))
      ip <<- ip + 2
      if (length(outputs) == 2) return(outputs)
    } else if (op == 5) {
      if (val(1, m1) != 0) ip <<- val(2, m2) else ip <<- ip + 3
    } else if (op == 6) {
      if (val(1, m1) == 0) ip <<- val(2, m2) else ip <<- ip + 3
    } else if (op == 7) {
      mem[mem[ip + 4] + 1] <<- if (val(1, m1) < val(2, m2)) 1 else 0
      ip <<- ip + 4
    } else if (op == 8) {
      mem[mem[ip + 4] + 1] <<- if (val(1, m1) == val(2, m2)) 1 else 0
      ip <<- ip + 4
    }
  }
}

while (!halted) {
  key <- paste(pos, collapse = ",")
  curr_color <- if (exists(key, envir = grid)) grid[[key]] else 0
  res <- run_intcode(curr_color)
  if (length(res) == 2) {
    grid[[key]] <- res[1]
    dir <- if (res[2] == 0) (dir + 3) %% 4 else (dir + 1) %% 4
    if (dir == 0) pos[2] <- pos[2] - 1
    else if (dir == 1) pos[1] <- pos[1] + 1
    else if (dir == 2) pos[2] <- pos[2] + 1
    else if (dir == 3) pos[1] <- pos[1] - 1
  }
}

cat(length(ls(grid)), "\n")
