
options(expressions = 10000)

run_intcode <- function(s, input_val) {
  while (TRUE) {
    instr <- s$mem[s$ip + 1]
    opcode <- instr %% 100
    m <- c((instr %/% 100) %% 10, (instr %/% 1000) %% 10, (instr %/% 10000) %% 10)
    
    get_ptr <- function(off) {
      mode <- m[off]
      p <- s$mem[s$ip + off + 1]
      if (mode == 0) addr <- p
      else if (mode == 1) addr <- s$ip + off
      else addr <- s$relative_base + p
      if (addr + 1 > length(s$mem)) {
        s$mem <- c(s$mem, rep(0, addr + 1 - length(s$mem) + 500))
      }
      return(addr + 1)
    }
    
    if (opcode == 99) return(list(halted = TRUE, output = NULL))
    if (opcode == 1) {
      p1 <- get_ptr(1); p2 <- get_ptr(2); p3 <- get_ptr(3)
      s$mem[p3] <- s$mem[p1] + s$mem[p2]
      s$ip <- s$ip + 4
    } else if (opcode == 2) {
      p1 <- get_ptr(1); p2 <- get_ptr(2); p3 <- get_ptr(3)
      s$mem[p3] <- s$mem[p1] * s$mem[p2]
      s$ip <- s$ip + 4
    } else if (opcode == 3) {
      s$mem[get_ptr(1)] <- input_val
      s$ip <- s$ip + 2
    } else if (opcode == 4) {
      out <- s$mem[get_ptr(1)]
      s$ip <- s$ip + 2
      return(list(halted = FALSE, output = out))
    } else if (opcode == 5) {
      v1 <- s$mem[get_ptr(1)]; v2 <- s$mem[get_ptr(2)]
      if (v1 != 0) s$ip <- v2 else s$ip <- s$ip + 3
    } else if (opcode == 6) {
      v1 <- s$mem[get_ptr(1)]; v2 <- s$mem[get_ptr(2)]
      if (v1 == 0) s$ip <- v2 else s$ip <- s$ip + 3
    } else if (opcode == 7) {
      v1 <- s$mem[get_ptr(1)]; v2 <- s$mem[get_ptr(2)]; p3 <- get_ptr(3)
      s$mem[p3] <- if (v1 < v2) 1 else 0
      s$ip <- s$ip + 4
    } else if (opcode == 8) {
      v1 <- s$mem[get_ptr(1)]; v2 <- s$mem[get_ptr(2)]; p3 <- get_ptr(3)
      s$mem[p3] <- if (v1 == v2) 1 else 0
      s$ip <- s$ip + 4
    } else if (opcode == 9) {
      s$relative_base <- s$relative_base + s$mem[get_ptr(1)]
      s$ip <- s$ip + 2
    }
  }
}

input_data <- as.numeric(scan("input.txt", sep = ",", quiet = TRUE))
mem <- numeric(max(length(input_data), 10000))
mem[1:length(input_data)] <- input_data
state <- new.env()
state$mem <- mem
state$ip <- 0
state$relative_base <- 0

grid <- matrix(0, 100, 100)
start_pos <- c(50, 50)
grid[start_pos[1], start_pos[2]] <- 2
oxygen_pos <- NULL

dx <- c(0, 0, -1, 1)
dy <- c(-1, 1, 0, 0)
opp <- c(2, 1, 4, 3)

explore <- function(x, y) {
  for (i in 1:4) {
    nx <- x + dx[i]
    ny <- y + dy[i]
    if (grid[nx, ny] == 0) {
      res <- run_intcode(state, i)
      if (res$output == 0) {
        grid[nx, ny] <<- 1
      } else {
        if (res$output == 2) {
          grid[nx, ny] <<- 3
          oxygen_pos <<- c(nx, ny)
        } else {
          grid[nx, ny] <<- 2
        }
        explore(nx, ny)
        run_intcode(state, opp[i])
      }
    }
  }
}

explore(start_pos[1], start_pos[2])

bfs <- function(start_node) {
  q <- list(list(pos = start_node, d = 0))
  visited <- matrix(FALSE, 100, 100)
  visited[start_node[1], start_node[2]] <- TRUE
  max_d <- 0
  dist_to_ox <- -1
  head <- 1
  while (head <= length(q)) {
    curr <- q[[head]]
    head <- head + 1
    max_d <- max(max_d, curr$d)
    if (grid[curr$pos[1], curr$pos[2]] == 3) dist_to_ox <- curr$d
    for (i in 1:4) {
      nx <- curr$pos[1] + dx[i]
      ny <- curr$pos[2] + dy[i]
      if (grid[nx, ny] > 1 && !visited[nx, ny]) {
        visited[nx, ny] <- TRUE
        q[[length(q) + 1]] <- list(pos = c(nx, ny), d = curr$d + 1)
      }
    }
  }
  return(list(dist_to_ox = dist_to_ox, max_d = max_d))
}

ans1 <- bfs(start_pos)
ans2 <- bfs(oxygen_pos)
cat(sprintf("Part 1: %d\n", ans1$dist_to_ox))
cat(sprintf("Part 2: %d\n", ans2$max_d))
