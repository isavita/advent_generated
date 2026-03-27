
input_str <- readLines("input.txt", warn = FALSE)[1]
target <- as.integer(unlist(strsplit(input_str, "")))
n_target <- length(target)
last_digit <- target[n_target]

scores <- integer(35000000)
scores[1] <- 3L
scores[2] <- 7L
len <- 2L
e1 <- 1L
e2 <- 2L

repeat {
  s <- scores[e1] + scores[e2]
  
  if (s >= 10L) {
    len <- len + 1L
    scores[len] <- 1L
    if (scores[len] == last_digit && len >= n_target) {
      if (all(scores[(len - n_target + 1L):len] == target)) break
    }
    
    len <- len + 1L
    scores[len] <- s - 10L
    if (scores[len] == last_digit && len >= n_target) {
      if (all(scores[(len - n_target + 1L):len] == target)) break
    }
  } else {
    len <- len + 1L
    scores[len] <- s
    if (scores[len] == last_digit && len >= n_target) {
      if (all(scores[(len - n_target + 1L):len] == target)) break
    }
  }
  
  e1 <- (e1 + scores[e1]) %% len + 1L
  e2 <- (e2 + scores[e2]) %% len + 1L
}

cat(len - n_target, "\n")
