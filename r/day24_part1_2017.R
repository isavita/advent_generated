components <- read.table("input.txt", sep = "/", col.names = c("a", "b"), header = FALSE)
max_strength <- 0

find_strongest_bridge <- function(components, used, port, strength) {
  if (strength > max_strength) {
    assign("max_strength", strength, envir = .GlobalEnv)
  }
  
  for (i in seq_along(components$a)) {
    if (used[i]) next
    if (components$a[i] == port || components$b[i] == port) {
      used[i] <- TRUE
      next_port <- ifelse(components$a[i] == port, components$b[i], components$a[i])
      find_strongest_bridge(components, used, next_port, strength + components$a[i] + components$b[i])
      used[i] <- FALSE
    }
  }
}

used <- rep(FALSE, nrow(components))
find_strongest_bridge(components, used, 0, 0)
print(max_strength)