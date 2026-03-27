
input <- readLines("input.txt")
tokens <- strsplit(input[2], ",")[[1]]
is_bus <- tokens != "x"
ids <- as.numeric(tokens[is_bus])
offs <- which(is_bus) - 1

t <- 0
s <- 1

for (i in seq_along(ids)) {
  while ((t + offs[i]) %% ids[i] != 0) {
    t <- t + s
  }
  s <- s * ids[i]
}

cat(sprintf("%.0f\n", t))
