read_input <- function() {
  gsub("[\n\r]", "", readLines("input.txt"))
}

move <- function(p, dir) {
  switch(dir,
         'N' = c(p[1], p[2] - 1),
         'S' = c(p[1], p[2] + 1),
         'E' = c(p[1] + 1, p[2]),
         'W' = c(p[1] - 1, p[2]),
         p)
}

build_map <- function(regex) {
  dm <- list()
  stack <- list()
  cp <- c(0, 0)
  
  for (c in strsplit(regex, "")[[1]]) {
    if (c == "(") {
      stack <- append(stack, list(cp))
    } else if (c == "|") {
      cp <- tail(stack, n=1)[[1]]
    } else if (c == ")") {
      cp <- tail(stack, n=1)[[1]]
      stack <- head(stack, -1)
    } else {
      np <- move(cp, c)
      if (is.null(dm[[paste(cp, collapse=",")]])) {
        dm[[paste(cp, collapse=",")]] <- list()
      }
      dm[[paste(cp, collapse=",")]][[paste(np, collapse=",")]] <- TRUE
      cp <- np
    }
  }
  dm
}

find_furthest_room <- function(dm) {
  visited <- list()
  queue <- list(c(0, 0))
  max_doors <- 0
  
  while (length(queue) > 0) {
    p <- queue[[1]]
    queue <- queue[-1]
    for (np in names(dm[[paste(p, collapse=",")]])) {
      if (is.null(visited[[np]])) {
        visited[[np]] <- (ifelse(is.null(visited[[paste(p, collapse=",")]]), 0, visited[[paste(p, collapse=",")]]) + 1)
        max_doors <- max(max_doors, visited[[np]])
        queue <- append(queue, list(as.numeric(unlist(strsplit(np, ",")))))
      }
    }
  }
  max_doors
}

regex <- read_input()
dm <- build_map(substr(regex, 2, nchar(regex) - 1))
max_doors <- find_furthest_room(dm)
print(max_doors)