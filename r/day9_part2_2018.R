
input <- readLines("input.txt", warn = FALSE)
matches <- regmatches(input, gregexpr("[0-9]+", input))[[1]]
players <- as.numeric(matches[1])
last_marble <- as.numeric(matches[2]) * 100

scores <- numeric(players)
next_ptr <- integer(last_marble + 1)
prev_ptr <- integer(last_marble + 1)

next_ptr[1] <- 1
prev_ptr[1] <- 1
current <- 1

for (m in 1:last_marble) {
  if (m %% 23 == 0) {
    p_idx <- (m - 1) %% players + 1
    for (j in 1:7) {
      current <- prev_ptr[current]
    }
    scores[p_idx] <- scores[p_idx] + m + (current - 1)
    
    p_node <- prev_ptr[current]
    n_node <- next_ptr[current]
    next_ptr[p_node] <- n_node
    prev_ptr[n_node] <- p_node
    current <- n_node
  } else {
    n1 <- next_ptr[current]
    n2 <- next_ptr[n1]
    
    new_node <- m + 1
    next_ptr[n1] <- new_node
    prev_ptr[new_node] <- n1
    next_ptr[new_node] <- n2
    prev_ptr[n2] <- new_node
    current <- new_node
  }
}

cat(sprintf("%.0f\n", max(scores)))
