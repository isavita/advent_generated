firewall <- read.table("input.txt", sep = ":", col.names = c("depth", "range"), header = FALSE, stringsAsFactors = FALSE)
firewall <- setNames(as.list(as.numeric(firewall$range)), as.character(firewall$depth))

severity <- 0
max_depth <- max(as.numeric(names(firewall)))

for (depth in 0:max_depth) {
  if (as.character(depth) %in% names(firewall)) {
    range <- firewall[[as.character(depth)]]
    if (range > 0 && depth %% (2 * (range - 1)) == 0) {
      severity <- severity + depth * range
    }
  }
}

print(severity)