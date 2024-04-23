firewall <- list()
file <- file("input.txt", "r")
while (TRUE) {
  line <- readLines(file, n = 1)
  if (length(line) == 0) break
  fields <- strsplit(line, ": ")[[1]]
  depth <- as.integer(fields[1])
  rng <- as.integer(fields[2])
  firewall[[as.character(depth)]] <- list(Range = rng, Position = 0, Direction = 1)
}
close(file) # Don't forget to close the file

passThrough <- function(firewall, delay) {
  for (depth in names(firewall)) {
    scanner <- firewall[[depth]]
    if ((as.integer(depth) + delay) %% (2 * (scanner$Range - 1)) == 0) return(FALSE)
  }
  return(TRUE)
}

delay <- 0
while (TRUE) {
  if (passThrough(firewall, delay)) break
  delay <- delay + 1
}

print(delay)