
options(scipen = 999)
lines <- readLines("input.txt")
init_str <- sub("initial state: ", "", lines[grep("initial state", lines)])
pots <- which(strsplit(init_str, "")[[1]] == "#") - 1

rules_lines <- lines[grep("=>", lines)]
rule_map <- logical(32)
for (rl in rules_lines) {
  parts <- strsplit(rl, " => ")[[1]]
  if (parts[2] == "#") {
    bits <- strsplit(parts[1], "")[[1]] == "#"
    idx <- sum(2^(4:0) * bits) + 1
    rule_map[idx] <- TRUE
  }
}

prev_sum <- 0
prev_pattern <- ""
target <- 50000000000

for (gen in 1:1000) {
  p_min <- min(pots)
  p_max <- max(pots)
  
  has_plant <- logical(p_max - p_min + 9)
  offset <- p_min - 4
  has_plant[pots - offset] <- TRUE
  
  new_pots <- integer(0)
  for (i in (p_min - 2):(p_max + 2)) {
    window <- has_plant[(i - 2 - offset):(i + 2 - offset)]
    idx <- sum(2^(4:0) * window) + 1
    if (rule_map[idx]) new_pots <- c(new_pots, i)
  }
  
  pots <- new_pots
  curr_sum <- sum(as.numeric(pots))
  curr_pattern <- paste0(pots - min(pots), collapse = ",")
  
  if (curr_pattern == prev_pattern) {
    diff <- curr_sum - prev_sum
    ans <- curr_sum + (target - gen) * diff
    cat(ans, "\n")
    break
  }
  
  prev_pattern <- curr_pattern
  prev_sum <- curr_sum
}
