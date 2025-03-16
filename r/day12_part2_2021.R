
find_paths <- function(graph, start, end, visited, small_twice = FALSE) {
  if (start == end) {
    return(1)
  }
  if (tolower(start) == start && start %in% visited) {
    if (small_twice) {
      return(0)
    }
    small_twice <- TRUE
  }
  visited <- c(visited, start)
  paths <- 0
  for (neighbor in graph[[start]]) {
    paths <- paths + find_paths(graph, neighbor, end, visited, small_twice)
  }
  return(paths)
}

main <- function() {
  lines <- readLines("input.txt")
  graph <- new.env(hash = TRUE)

  for (line in lines) {
    parts <- strsplit(line, "-")[[1]]
    a <- parts[1]
    b <- parts[2]

    if (a != "end" && b != "start") {
      if (exists(a, envir = graph)) {
        graph[[a]] <- c(graph[[a]], b)
      } else {
        graph[[a]] <- c(b)
      }
    }
    if (b != "end" && a != "start") {
      if (exists(b, envir = graph)) {
        graph[[b]] <- c(graph[[b]], a)
      } else {
        graph[[b]] <- c(a)
      }
    }
  }
  
    result1 <- find_paths(graph, "start", "end", c())
  cat(result1, "\n")
  
  result2 <- find_paths(graph, "start", "end", c(), TRUE)
  cat(result2, "\n")
}

main()
