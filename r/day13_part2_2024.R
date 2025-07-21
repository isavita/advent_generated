
main <- function() {
  offset <- 10000000000000
  machines <- read_input("input.txt")
  results <- numeric()
  for (m in machines) {
    cost <- solve_machine(m[1], m[2], m[3], m[4], m[5] + offset, m[6] + offset)
    if (cost >= 0) results <- c(results, cost)
  }
  cat(if (length(results) == 0) "0 0\n" else paste(length(results), sum(results), "\n"))
}

read_input <- function(filename) {
  txt <- readLines(filename)
  txt <- txt[txt != ""]
  machines <- list()
  for (i in seq(1, length(txt), 3)) {
    a <- parse_line(txt[i])
    b <- parse_line(txt[i + 1])
    p <- parse_prize(txt[i + 2])
    machines[[length(machines) + 1]] <- c(a, b, p)
  }
  machines
}

parse_line <- function(s) {
  g <- gregexpr("[-+]?\\d+", s)
  as.integer(regmatches(s, g)[[1]])
}

parse_prize <- function(s) {
  g <- gregexpr("\\d+", s)
  as.integer(regmatches(s, g)[[1]])
}

solve_machine <- function(ax, ay, bx, by, px, py) {
  D <- ax * by - ay * bx
  if (D == 0) return(-1)
  numA <- px * by - py * bx
  numB <- -px * ay + py * ax
  if (numA %% D != 0 || numB %% D != 0) return(-1)
  a <- numA %/% D
  b <- numB %/% D
  if (a < 0 || b < 0) return(-1)
  3 * a + b
}

main()
