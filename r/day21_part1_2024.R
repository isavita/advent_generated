
find_position <- function(mat, ch) {
  for (i in 1:length(mat)) {
    for (j in 1:nchar(mat[i])) {
      if (substr(mat[i], j, j) == ch) {
        return(c(i, j))
      }
    }
  }
  return(c(-1, -1))
}

ok <- function(mat, st, seq) {
  curr_i <- st[1]
  curr_j <- st[2]
  for (k in 1:nchar(seq)) {
    ch <- substr(seq, k, k)
    if (substr(mat[curr_i], curr_j, curr_j) == ' ') {
      return(FALSE)
    }
    if (ch == '^') {
      curr_i <- curr_i - 1
    } else if (ch == 'v') {
      curr_i <- curr_i + 1
    } else if (ch == '<') {
      curr_j <- curr_j - 1
    } else if (ch == '>') {
      curr_j <- curr_j + 1
    }
    if (curr_i < 1 || curr_i > length(mat) || curr_j < 1 || curr_j > nchar(mat[1])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

generate_moves <- function(position, objective, pad) {
    obj_pos <- find_position(pad, objective)
    obj_i <- obj_pos[1]
    obj_j <- obj_pos[2]
    pos_i <- position[1]
    pos_j <- position[2]
    
    ret <- ""
    if (pos_j > obj_j) {
        ret <- paste0(ret, strrep("<", pos_j - obj_j))
    }
    if (pos_i > obj_i) {
        ret <- paste0(ret, strrep("^", pos_i - obj_i))
    }
    if (pos_i < obj_i) {
        ret <- paste0(ret, strrep("v", obj_i - pos_i))
    }
    if (pos_j < obj_j) {
        ret <- paste0(ret, strrep(">", obj_j - pos_j))
    }
    
    if (!ok(pad, position, ret)) {
        ret <- ""
        if (pos_j < obj_j) {
            ret <- paste0(ret, strrep(">", obj_j - pos_j))
        }
        if (pos_i > obj_i) {
            ret <- paste0(ret, strrep("^", pos_i - obj_i))
        }
        if (pos_i < obj_i) {
            ret <- paste0(ret, strrep("v", obj_i - pos_i))
        }
        if (pos_j > obj_j) {
            ret <- paste0(ret, strrep("<", pos_j - obj_j))
        }
    }
    return(ret)
}

solve <- function(code, robots, key_pad, robot_pad, max_robots, memo) {
  if (robots <= 0) {
    return(nchar(code))
  }
  
  state <- paste(code, robots, sep = "|")
  if (state %in% names(memo)) {
    return(memo[[state]])
  }
  
  ret <- 0
  pos_i <- 4
  pos_j <- 3
  if (robots != max_robots) {
    pos_i <- 1
  }
  
  for (i in 1:nchar(code)) {
    ch <- substr(code, i, i)
    if (robots == max_robots) {
      moves <- generate_moves(c(pos_i, pos_j), ch, key_pad)
      pos <- find_position(key_pad, ch)
      pos_i <- pos[1]
      pos_j <- pos[2]
    } else {
      moves <- generate_moves(c(pos_i, pos_j), ch, robot_pad)
      pos <- find_position(robot_pad, ch)
      pos_i <- pos[1]
      pos_j <- pos[2]
    }
    memo <- solve(paste0(moves, "A"), robots - 1, key_pad, robot_pad, max_robots, memo)
    ret <- ret + memo
  }
  
  memo[[state]] <- ret
  return(ret)
}

main <- function() {
  content <- readLines("input.txt")
  
  max_robots <- 3
  key_pad <- c("789", "456", "123", " 0A")
  robot_pad <- c(" ^A", "<v>")
  
  ret <- 0
  codes <- content
  
  for (code in codes) {
    code <- trimws(code)
    if (nchar(code) == 0) {
      next
    }
    
    numeric_part <- 0
    for (i in 1:nchar(code)) {
        char <- substr(code, i, i)
        if (grepl("[0-9]", char)) {
            numeric_part <- numeric_part * 10 + as.integer(char)
        }
    }

    memo <- list()
    sv <- solve(code, max_robots, key_pad, robot_pad, max_robots, memo)
    ret <- ret + sv * numeric_part
  }
  
  cat(ret, "\n")
}

main()
