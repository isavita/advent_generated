options(expressions = 10000)

key_pad <- matrix(c('7','8','9','4','5','6','1','2','3',' ','0','A'), nrow = 4, ncol = 3, byrow = TRUE)
robot_pad <- matrix(c(' ','^','A','<','v','>'), nrow = 2, ncol = 3, byrow = TRUE)
memo <- new.env(hash = TRUE, parent = emptyenv())

get_pos <- function(ch, pad) {
  pos <- which(pad == ch, arr.ind = TRUE)
  return(c(pos[1, 1], pos[1, 2]))
}

is_ok <- function(r, c, seq, pad) {
  if (seq == "") return(TRUE)
  chars <- strsplit(seq, "")[[1]]
  curr_r <- r
  curr_c <- c
  rows <- nrow(pad)
  cols <- ncol(pad)
  for (m in chars) {
    if (curr_r < 1 || curr_r > rows || curr_c < 1 || curr_c > cols || pad[curr_r, curr_c] == ' ') {
      return(FALSE)
    }
    if (m == '^') curr_r <- curr_r - 1
    else if (m == 'v') curr_r <- curr_r + 1
    else if (m == '<') curr_c <- curr_c - 1
    else if (m == '>') curr_c <- curr_c + 1
  }
  TRUE
}

solve_rec <- function(code, robots) {
  key <- paste0(code, "_", robots)
  if (exists(key, envir = memo)) return(get(key, envir = memo))
  
  if (robots == 0) return(nchar(code))
  
  pad <- if (robots == 26) key_pad else robot_pad
  curr_pos <- get_pos('A', pad)
  curr_r <- curr_pos[1]
  curr_c <- curr_pos[2]
  
  res <- 0
  code_chars <- strsplit(code, "")[[1]]
  for (ch in code_chars) {
    obj_pos <- get_pos(ch, pad)
    obj_r <- obj_pos[1]
    obj_c <- obj_pos[2]
    
    m <- ""
    if (curr_c > obj_c) m <- paste0(m, strrep("<", curr_c - obj_c))
    if (curr_r > obj_r) m <- paste0(m, strrep("^", curr_r - obj_r))
    if (curr_r < obj_r) m <- paste0(m, strrep("v", obj_r - curr_r))
    if (curr_c < obj_c) m <- paste0(m, strrep(">", obj_c - curr_c))
    
    if (!is_ok(curr_r, curr_c, m, pad)) {
      m <- ""
      if (curr_c < obj_c) m <- paste0(m, strrep(">", obj_c - curr_c))
      if (curr_r > obj_r) m <- paste0(m, strrep("^", curr_r - obj_r))
      if (curr_r < obj_r) m <- paste0(m, strrep("v", obj_r - curr_r))
      if (curr_c > obj_c) m <- paste0(m, strrep("<", curr_c - obj_c))
    }
    
    res <- res + solve_rec(paste0(m, "A"), robots - 1)
    curr_r <- obj_r
    curr_c <- obj_c
  }
  
  assign(key, res, envir = memo)
  return(res)
}

input_lines <- readLines("input.txt", warn = FALSE)
total_res <- 0
for (line in input_lines) {
  line <- trimws(line)
  if (line == "") next
  num_part <- as.numeric(gsub("[^0-9]", "", line))
  total_res <- total_res + (solve_rec(line, 26) * num_part)
}
cat(sprintf("%.0f", total_res), "\n")