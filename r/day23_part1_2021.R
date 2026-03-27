
lines <- readLines("input.txt")
depth <- length(lines) - 3
room_data <- matrix(NA, nrow = depth, ncol = 4)
for (i in 1:depth) {
  line_chars <- strsplit(lines[i + 2], "")[[1]]
  room_data[i, ] <- line_chars[line_chars %in% c("A", "B", "C", "D")]
}

initial_state <- paste0(c(rep(".", 11), as.vector(room_data)), collapse = "")
target_state <- paste0(c(rep(".", 11), rep(c("A", "B", "C", "D"), each = depth)), collapse = "")
energy_map <- c(A = 1, B = 10, C = 100, D = 1000)
room_cols <- c(A = 3, B = 5, C = 7, D = 9)

get_moves <- function(s_str) {
  s <- strsplit(s_str, "")[[1]]
  room_to_hallway <- list()
  
  for (i in 1:11) {
    if (s[i] == ".") next
    type <- s[i]
    r_idx <- match(type, names(room_cols))
    r_indices <- 11 + (r_idx - 1) * depth + (1:depth)
    room_contents <- s[r_indices]
    
    if (all(room_contents %in% c(".", type))) {
      target_h <- room_cols[r_idx]
      path <- if (target_h > i) (i + 1):target_h else (i - 1):target_h
      if (!any(s[path] != ".")) {
        level <- max(which(room_contents == "."))
        new_s <- s
        new_s[r_indices[level]] <- type
        new_s[i] <- "."
        return(list(list(state = paste0(new_s, collapse = ""), cost = (length(path) + level) * energy_map[type])))
      }
    }
  }
  
  for (r_idx in 1:4) {
    r_indices <- 11 + (r_idx - 1) * depth + (1:depth)
    room_contents <- s[r_indices]
    if (all(room_contents == ".")) next
    level <- min(which(room_contents != "."))
    type <- room_contents[level]
    if (type == names(room_cols)[r_idx] && all(room_contents[level:depth] == type)) next
    
    entrance_h <- room_cols[r_idx]
    for (h_idx in c(1, 2, 4, 6, 8, 10, 11)) {
      path <- entrance_h:h_idx
      if (!any(s[path] != ".")) {
        new_s <- s
        new_s[h_idx] <- type
        new_s[r_indices[level]] <- "."
        room_to_hallway[[length(room_to_hallway) + 1]] <- list(
          state = paste0(new_s, collapse = ""),
          cost = (length(path) - 1 + level) * energy_map[type]
        )
      }
    }
  }
  room_to_hallway
}

pq_cost <- 0
pq_state <- initial_state
seen <- new.env(hash = TRUE)
seen[[initial_state]] <- 0

while (length(pq_state) > 0) {
  curr_cost <- pq_cost[1]; curr_state <- pq_state[1]
  pq_cost <- pq_cost[-1]; pq_state <- pq_state[-1]
  
  if (curr_state == target_state) {
    cat(curr_cost, "\n")
    break
  }
  if (curr_cost > seen[[curr_state]]) next
  
  moves <- get_moves(curr_state)
  for (m in moves) {
    new_cost <- curr_cost + m$cost
    if (!exists(m$state, envir = seen) || new_cost < seen[[m$state]]) {
      seen[[m$state]] <- new_cost
      idx <- findInterval(new_cost, pq_cost)
      pq_cost <- append(pq_cost, new_cost, idx)
      pq_state <- append(pq_state, m$state, idx)
    }
  }
}
