
intcode <- function(m_vec, inputs) {
  m <- new.env()
  for (i in seq_along(m_vec)) m[[as.character(i-1)]] <- m_vec[i]
  p <- 0; rb <- 0; in_p <- 1; outputs <- c()
  get_v <- function(mode, val) {
    if (mode == 1) return(val)
    addr <- if (mode == 0) val else rb + val
    res <- m[[as.character(addr)]]
    if (is.null(res)) 0 else res
  }
  set_v <- function(mode, val, v) {
    addr <- if (mode == 0) val else rb + val
    m[[as.character(addr)]] <<- v
  }
  while (TRUE) {
    instr <- m[[as.character(p)]]; if (is.null(instr)) instr <- 0
    opcode <- instr %% 100
    m1 <- (instr %/% 100) %% 10; m2 <- (instr %/% 1000) %% 10; m3 <- (instr %/% 10000) %% 10
    if (opcode == 99) break
    p_vals <- c(); for(i in 1:3) {
      v <- m[[as.character(p+i)]]; p_vals <- c(p_vals, if(is.null(v)) 0 else v)
    }
    if (opcode == 1) {
      set_v(m3, p_vals[3], get_v(m1, p_vals[1]) + get_v(m2, p_vals[2])); p <- p + 4
    } else if (opcode == 2) {
      set_v(m3, p_vals[3], get_v(m1, p_vals[1]) * get_v(m2, p_vals[2])); p <- p + 4
    } else if (opcode == 3) {
      if (in_p > length(inputs)) break
      set_v(m1, p_vals[1], inputs[in_p]); in_p <- in_p + 1; p <- p + 2
    } else if (opcode == 4) {
      outputs <- c(outputs, get_v(m1, p_vals[1])); p <- p + 2
    } else if (opcode == 5) {
      if (get_v(m1, p_vals[1]) != 0) p <- get_v(m2, p_vals[2]) else p <- p + 3
    } else if (opcode == 6) {
      if (get_v(m1, p_vals[1]) == 0) p <- get_v(m2, p_vals[2]) else p <- p + 3
    } else if (opcode == 7) {
      set_v(m3, p_vals[3], as.numeric(get_v(m1, p_vals[1]) < get_v(m2, p_vals[2]))); p <- p + 4
    } else if (opcode == 8) {
      set_v(m3, p_vals[3], as.numeric(get_v(m1, p_vals[1]) == get_v(m2, p_vals[2]))); p <- p + 4
    } else if (opcode == 9) {
      rb <- rb + get_v(m1, p_vals[1]); p <- p + 2
    }
  }
  outputs
}

input_raw <- readLines("input.txt", warn=FALSE)
prog <- as.numeric(strsplit(input_raw, ",")[[1]])
grid_chars <- intcode(prog, c())
grid_lines <- strsplit(intToUtf8(grid_chars), "\n")[[1]]
grid_lines <- grid_lines[nchar(grid_lines) > 0]
grid <- do.call(rbind, strsplit(grid_lines, ""))

ans1 <- 0
for (r in 2:(nrow(grid)-1)) {
  for (c in 2:(ncol(grid)-1)) {
    if (grid[r,c] == '#' && all(grid[c(r-1,r+1),c] == '#') && all(grid[r,c(c-1,c+1)] == '#'))
      ans1 <- ans1 + (r-1)*(c-1)
  }
}
cat(sprintf("%d\n", ans1))

start_char <- grid[grid %in% c('^', 'v', '<', '>')]
pos <- which(grid == start_char, arr.ind=TRUE)
r <- pos[1]; c <- pos[2]; d <- match(start_char, c('^', '>', 'v', '<'))
move <- function(p, d) {
  if (d == 1) return(c(p[1]-1, p[2])) else if (d == 2) return(c(p[1], p[2]+1))
  else if (d == 3) return(c(p[1]+1, p[2])) else return(c(p[1], p[2]-1))
}
turn <- function(d, side) if (side == 'L') (if (d == 1) 4 else d - 1) else (if (d == 4) 1 else d + 1)

path <- c()
repeat {
  dL <- turn(d, 'L'); nxtL <- move(c(r,c), dL)
  dR <- turn(d, 'R'); nxtR <- move(c(r,c), dR)
  if (nxtL[1] >= 1 && nxtL[1] <= nrow(grid) && nxtL[2] >= 1 && nxtL[2] <= ncol(grid) && grid[nxtL[1], nxtL[2]] == '#') {
    path <- c(path, 'L'); d <- dL
  } else if (nxtR[1] >= 1 && nxtR[1] <= nrow(grid) && nxtR[2] >= 1 && nxtR[2] <= ncol(grid) && grid[nxtR[1], nxtR[2]] == '#') {
    path <- c(path, 'R'); d <- dR
  } else break
  steps <- 0
  repeat {
    nxt <- move(c(r,c), d)
    if (nxt[1] >= 1 && nxt[1] <= nrow(grid) && nxt[2] >= 1 && nxt[2] <= ncol(grid) && grid[nxt[1], nxt[2]] == '#') {
      r <- nxt[1]; c <- nxt[2]; steps <- steps + 1
    } else break
  }
  path <- c(path, as.character(steps))
}

get_main <- function(p, abc) {
  res <- c(); i <- 1
  while (i <= length(p)) {
    found <- FALSE
    for (idx in seq_along(abc)) {
      pat <- abc[[idx]]; len <- length(pat)
      if (i+len-1 <= length(p) && all(p[i:(i+len-1)] == pat)) {
        res <- c(res, LETTERS[idx]); i <- i + len; found <- TRUE; break
      }
    }
    if (!found) return(NULL)
  }
  m_str <- paste(res, collapse=",")
  if (nchar(m_str) <= 20) m_str else NULL
}

solve_path <- function(tokens, abc) {
  if (length(tokens) == 0) {
    m_str <- get_main(path, abc)
    if (!is.null(m_str)) return(list(abc=abc, main=m_str)) else return(NULL)
  }
  for (pat in abc) if (length(tokens) >= length(pat) && all(tokens[1:length(pat)] == pat)) {
    res <- solve_path(tokens[-(1:length(pat))], abc)
    if (!is.null(res)) return(res)
  }
  if (length(abc) < 3) for (len in 1:min(10, length(tokens))) {
    new_pat <- tokens[1:len]
    if (nchar(paste(new_pat, collapse=",")) > 20) break
    res <- solve_path(tokens[-(1:len)], c(abc, list(new_pat)))
    if (!is.null(res)) return(res)
  }
  NULL
}

sol <- solve_path(path, list())
prog[1] <- 2
input_str <- paste0(sol$main, "\n", paste(sol$abc[[1]], collapse=","), "\n", 
                    paste(sol$abc[[2]], collapse=","), "\n", 
                    paste(sol$abc[[3]], collapse=","), "\n", "n\n")
ans2 <- intcode(prog, utf8ToInt(input_str))
cat(sprintf("%0.0f\n", tail(ans2, 1)))
