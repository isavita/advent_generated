
parse_line <- function(s) {
  s <- gsub("X\\+|Y\\+|X=|Y=", "", s)
  parts <- as.integer(trimws(strsplit(s, ",")[[1]]))
  return(parts)
}

parse_prize <- function(s) {
  s <- gsub("X=|Y=", "", s)
  parts <- as.integer(trimws(strsplit(s, ",")[[1]]))
  return(parts)
}

parse_machine <- function(lines) {
  ax <- ay <- bx <- by <- px <- py <- NA
  for (l in lines) {
    if (startsWith(l, "Button A:")) {
      coords <- parse_line(substring(l, 10))
      ax <- coords[1]
      ay <- coords[2]
    } else if (startsWith(l, "Button B:")) {
      coords <- parse_line(substring(l, 10))
      bx <- coords[1]
      by <- coords[2]
    } else if (startsWith(l, "Prize:")) {
      coords <- parse_prize(substring(l, 7))
      px <- coords[1]
      py <- coords[2]
    }
  }
  return(list(ax = ax, ay = ay, bx = bx, by = by, px = px, py = py))
}

solve_machine <- function(m) {
  min_cost <- Inf
  a_seq <- 0:100
  b_seq <- 0:100
  
  x <- outer(m$ax * a_seq, m$bx * b_seq, "+")
  y <- outer(m$ay * a_seq, m$by * b_seq, "+")
  
  matches <- which(x == m$px & y == m$py, arr.ind = TRUE)
  
  if(nrow(matches) > 0){
    costs <- 3 * a_seq[matches[,1]] + b_seq[matches[,2]]
    min_cost <- min(costs)
  }
  
  if (is.infinite(min_cost)) {
    return(-1)
  } else {
    return(min_cost)
  }
}

read_input <- function(filename) {
  lines <- readLines(filename)
  machines <- list()
  current_machine_lines <- list()
  for (line in lines) {
    line <- trimws(line)
    if (line == "") {
      if (length(current_machine_lines) > 0) {
        machines <- append(machines, list(parse_machine(current_machine_lines)))
        current_machine_lines <- list()
      }
    } else {
      current_machine_lines <- append(current_machine_lines, line)
    }
  }
  if (length(current_machine_lines) > 0) {
    machines <- append(machines, list(parse_machine(current_machine_lines)))
  }
  return(machines)
}

machines <- read_input("input.txt")
results <- integer()
for (m in machines) {
  cost <- solve_machine(m)
  if (cost >= 0) {
    results <- c(results, cost)
  }
}

if (length(results) == 0) {
  cat("0 0\n")
} else {
  cat(length(results), sum(results), "\n")
}
