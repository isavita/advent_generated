
options(scipen = 999)
lines <- readLines("input.txt")
r1 <- strsplit(gsub("[ #]", "", lines[3]), "")[[1]]
r4 <- strsplit(gsub("[ #]", "", lines[4]), "")[[1]]

start_s <- utf8ToInt(paste0(c(rep(".", 7), 
                             r1[1], "D", "D", r4[1],
                             r1[2], "C", "B", r4[2],
                             r1[3], "B", "A", r4[3],
                             r1[4], "A", "C", r4[4]), collapse = ""))
target_s <- utf8ToInt(".......AAAABBBBCCCCDDDD")

costs <- c(1, 10, 100, 1000)
hall_cols <- c(1, 2, 4, 6, 8, 10, 11)
room_cols <- c(3, 5, 7, 9)
room_idxs <- list(8:11, 12:15, 16:19, 20:23)

hall_path_idxs <- list()
for (h in 1:7) {
  hall_path_idxs[[h]] <- list()
  for (r in 1:4) {
    h_c <- hall_cols[h]; r_c <- room_cols[r]
    hall_path_idxs[[h]][[r]] <- which((hall_cols > h_c & hall_cols < r_c) | (hall_cols < h_c & hall_cols > r_c))
  }
}

h_cost <- numeric(1000000); h_state <- character(1000000); h_n <- 0
push_heap <- function(c, s) {
  h_n <<- h_n + 1; h_cost[h_n] <<- c; h_state[h_n] <<- s; i <- h_n
  while(i > 1 && h_cost[i] < h_cost[i %/% 2]) {
    tc <- h_cost[i]; h_cost[i] <<- h_cost[i %/% 2]; h_cost[i %/% 2] <<- tc
    ts <- h_state[i]; h_state[i] <<- h_state[i %/% 2]; h_state[i %/% 2] <<- ts
    i <- i %/% 2
  }
}
pop_heap <- function() {
  res <- list(cost = h_cost[1], state = h_state[1])
  h_cost[1] <<- h_cost[h_n]; h_state[1] <<- h_state[h_n]; h_n <<- h_n - 1; i <- 1
  while(TRUE) {
    l <- 2*i; r <- 2*i + 1; s <- i
    if(l <= h_n && h_cost[l] < h_cost[s]) s <- l
    if(r <= h_n && h_cost[r] < h_cost[s]) s <- r
    if(s != i) {
      tc <- h_cost[i]; h_cost[i] <<- h_cost[s]; h_cost[s] <<- tc
      ts <- h_state[i]; h_state[i] <<- h_state[s]; h_state[s] <<- ts
      i <- s
    } else break
  }
  res
}

visited <- new.env(hash = TRUE, parent = emptyenv())
push_heap(0, intToUtf8(start_s))

while(h_n > 0) {
  curr <- pop_heap(); if(exists(curr$state, envir = visited) && visited[[curr$state]] <= curr$cost) next
  visited[[curr$state]] <- curr$cost; s <- utf8ToInt(curr$state)
  if(all(s == target_s)) { cat(curr$cost, "\n"); break }
  
  found_imm <- FALSE
  for(i in 1:7) {
    if(s[i] == 46) next
    type <- s[i] - 64; r_idx_list <- room_idxs[[type]]; r_spots <- s[r_idx_list]
    if(all(r_spots == 46 | r_spots == (64 + type))) {
      if(all(s[hall_path_idxs[[i]][[type]]] == 46)) {
        deepest <- max(which(r_spots == 46)); dest <- r_idx_list[deepest]
        dist <- deepest + abs(hall_cols[i] - room_cols[type])
        ns <- s; ns[dest] <- s[i]; ns[i] <- 46; push_heap(curr$cost + dist * costs[type], intToUtf8(ns))
        found_imm <- TRUE; break
      }
    }
  }
  if(found_imm) next
  
  for(i in 8:23) {
    if(s[i] == 46) next
    r_type <- (i - 8) %/% 4 + 1; type <- s[i] - 64; r_range <- room_idxs[[r_type]]
    below <- r_range[r_range > i]; settled <- s[i] == (64 + r_type) && (length(below) == 0 || all(s[below] == (64 + r_type)))
    if(settled) next
    above <- r_range[r_range < i]; if(any(s[above] != 46)) next
    
    depth <- (i - r_range[1]) + 1
    for(h in 1:7) {
      if(s[h] == 46 && all(s[hall_path_idxs[[h]][[r_type]]] == 46)) {
        dist <- depth + abs(hall_cols[h] - room_cols[r_type])
        ns <- s; ns[h] <- s[i]; ns[i] <- 46; push_heap(curr$cost + dist * costs[type], intToUtf8(ns))
      }
    }
  }
}
