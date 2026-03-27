
options(scipen = 999)

gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

lcm <- function(a, b) {
  if (a == 0 || b == 0) return(0)
  return((a / gcd(a, b)) * b)
}

input <- readLines("input.txt")
instructions <- strsplit(input[1], "")[[1]]
n_instr <- length(instructions)

node_map <- list()
for (i in 3:length(input)) {
  line <- input[i]
  if (nchar(line) < 16) next
  name <- substr(line, 1, 3)
  left <- substr(line, 8, 10)
  right <- substr(line, 13, 15)
  node_map[[name]] <- c(L = left, R = right)
}

starts <- names(node_map)[grep("A$", names(node_map))]
if (length(starts) == 0) {
  cat(0, "\n")
  quit()
}

steps_needed <- numeric(length(starts))

for (i in seq_along(starts)) {
  curr <- starts[i]
  step_count <- 0
  while (substr(curr, 3, 3) != "Z") {
    dir <- instructions[(step_count %% n_instr) + 1]
    curr <- node_map[[curr]][dir]
    step_count <- step_count + 1
  }
  steps_needed[i] <- step_count
}

result <- steps_needed[1]
if (length(steps_needed) > 1) {
  for (i in 2:length(steps_needed)) {
    result <- lcm(result, steps_needed[i])
  }
}

cat(result, "\n")
