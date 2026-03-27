options(scipen = 999)

run_robot <- function(prog, start_color) {
  mem <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(prog)) mem[[as.character(i - 1)]] <- prog[i]
  
  get_v <- function(a) {
    res <- mem[[as.character(a)]]
    if (is.null(res)) 0 else res
  }
  
  set_v <- function(a, v) mem[[as.character(a)]] <- v
  
  ip <- 0
  rb <- 0
  curr_pos <- c(0, 0)
  curr_dir <- 0
  dx <- c(0, 1, 0, -1)
  dy <- c(-1, 0, 1, 0)
  
  panels <- new.env(hash = TRUE, parent = emptyenv())
  
  get_color <- function(pos) {
    key <- paste0(pos[1], ",", pos[2])
    res <- panels[[key]]
    if (is.null(res)) {
      if (all(pos == c(0, 0))) return(start_color)
      return(0)
    }
    res
  }
  
  outputs <- numeric(0)
  
  while (TRUE) {
    instr <- get_v(ip)
    opcode <- instr %% 100
    m1 <- instr %/% 100 %% 10
    m2 <- instr %/% 1000 %% 10
    m3 <- instr %/% 10000 %% 10
    
    if (opcode == 99) break
    
    if (opcode == 1) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      p3 <- get_v(ip + 3); a3 <- if (m3 == 0) p3 else rb + p3
      set_v(a3, v1 + v2)
      ip <- ip + 4
    } else if (opcode == 2) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      p3 <- get_v(ip + 3); a3 <- if (m3 == 0) p3 else rb + p3
      set_v(a3, v1 * v2)
      ip <- ip + 4
    } else if (opcode == 3) {
      a1 <- if (m1 == 0) get_v(ip + 1) else rb + get_v(ip + 1)
      set_v(a1, get_color(curr_pos))
      ip <- ip + 2
    } else if (opcode == 4) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      outputs <- c(outputs, v1)
      if (length(outputs) == 2) {
        panels[[paste0(curr_pos[1], ",", curr_pos[2])]] <- outputs[1]
        if (outputs[2] == 0) curr_dir <- (curr_dir + 3) %% 4
        else curr_dir <- (curr_dir + 1) %% 4
        curr_pos <- curr_pos + c(dx[curr_dir + 1], dy[curr_dir + 1])
        outputs <- numeric(0)
      }
      ip <- ip + 2
    } else if (opcode == 5) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      if (v1 != 0) ip <- v2 else ip <- ip + 3
    } else if (opcode == 6) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      if (v1 == 0) ip <- v2 else ip <- ip + 3
    } else if (opcode == 7) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      p3 <- get_v(ip + 3); a3 <- if (m3 == 0) p3 else rb + p3
      set_v(a3, if (v1 < v2) 1 else 0)
      ip <- ip + 4
    } else if (opcode == 8) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      p2 <- get_v(ip + 2); v2 <- if (m2 == 0) get_v(p2) else if (m2 == 1) p2 else get_v(rb + p2)
      p3 <- get_v(ip + 3); a3 <- if (m3 == 0) p3 else rb + p3
      set_v(a3, if (v1 == v2) 1 else 0)
      ip <- ip + 4
    } else if (opcode == 9) {
      p1 <- get_v(ip + 1); v1 <- if (m1 == 0) get_v(p1) else if (m1 == 1) p1 else get_v(rb + p1)
      rb <- rb + v1
      ip <- ip + 2
    }
  }
  as.list(panels)
}

prog <- scan("input.txt", sep = ",", quiet = TRUE)

p1 <- run_robot(prog, 0)
cat(sprintf("Part One: %d\n", length(p1)))

p2 <- run_robot(prog, 1)
cat("Part Two:\n")
coords <- do.call(rbind, lapply(strsplit(names(p2), ","), as.numeric))
for (y in min(coords[, 2]):max(coords[, 2])) {
  row <- ""
  for (x in min(coords[, 1]):max(coords[, 1])) {
    color <- p2[[paste0(x, ",", y)]]
    row <- paste0(row, if (!is.null(color) && color == 1) "#" else " ")
  }
  cat(row, "\n")
}