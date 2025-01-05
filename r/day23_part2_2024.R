
graph <- list()
nodes <- character()

con <- file("input.txt", "r")
while(TRUE) {
  line <- readLines(con, n = 1)
  if (length(line) == 0) {
    break
  }
  parts <- strsplit(line, "-")[[1]]
  if (length(parts) != 2) {
    next
  }
  a <- parts[1]
  b <- parts[2]
  if (is.null(graph[[a]])) {
    graph[[a]] <- character()
  }
  if (is.null(graph[[b]])) {
    graph[[b]] <- character()
  }
  graph[[a]] <- c(graph[[a]], b)
  graph[[b]] <- c(graph[[b]], a)
  nodes <- unique(c(nodes, a, b))
}
close(con)

best_clique <- character()

bron_kerbosch <- function(R, P, X) {
  if (length(P) == 0 && length(X) == 0) {
    if (length(R) > length(best_clique)) {
      best_clique <<- R
    }
    return()
  }
  for (v in P) {
    neighbors <- graph[[v]]
    bron_kerbosch(
      union(R, v),
      intersect(P, neighbors),
      intersect(X, neighbors)
    )
    P <- setdiff(P, v)
    X <- union(X, v)
  }
}

intersect <- function(a, b) {
  if(is.null(b)) return(character())
  intersect(a, names(b))
}

intersect <- function(a, b) {
  if(is.null(b)) return(character())
  a[a %in% b]
}

union <- function(a, x) {
  unique(c(a, x))
}

bron_kerbosch(character(), nodes, character())
cat(paste(sort(best_clique), collapse = ","), "\n")
