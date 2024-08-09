build_map <- function(regex) {
  dm <- list()
  stack <- list()
  cp <- c(0, 0)

  for (c in unlist(strsplit(regex, ""))) {
    if (c == "(") {
      stack <- append(stack, list(cp))
    } else if (c == "|") {
      cp <- tail(stack, n = 1)[[1]]
    } else if (c == ")") {
      cp <- tail(stack, n = 1)[[1]]
      stack <- head(stack, -1)
    } else {
      np <- move(cp, c)
      dm_key <- paste(cp, collapse = ",")
      if (is.null(dm[[dm_key]])) {
        dm[[dm_key]] <- list()
      }
      dm[[dm_key]][[paste(np, collapse = ",")]] <- TRUE
      cp <- np
    }
  }
  dm
}

move <- function(p, dir) {
  switch(dir,
         N = c(p[1], p[2] - 1),
         S = c(p[1], p[2] + 1),
         E = c(p[1] + 1, p[2]),
         W = c(p[1] - 1, p[2]))
}

count_rooms <- function(dm, min_doors) {
  visited <- list()
  queue <- list(c(0, 0))
  room_count <- 0

  while (length(queue) > 0) {
    p <- queue[[1]]
    queue <- queue[-1]
    p_key <- paste(p, collapse = ",")
    for (np in names(dm[[p_key]])) {
      if (!np %in% names(visited)) {
        visited[[np]] <- (ifelse(is.null(visited[[p_key]]), 0, visited[[p_key]])) + 1
        if (visited[[np]] >= min_doors) {
          room_count <- room_count + 1
        }
        queue <- append(queue, list(as.numeric(unlist(strsplit(np, ",")))))
      }
    }
  }
  room_count
}

data <- readLines("input.txt")
regex <- data[[1]]
dm <- build_map(substr(regex, 2, nchar(regex) - 1))
rooms <- count_rooms(dm, 1000)
print(rooms)