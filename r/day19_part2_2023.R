
options(scipen = 999)
lines <- readLines("input.txt")
blank <- which(lines == "")[1]
wf_lines <- lines[1:(blank - 1)]

wfs <- list()
for (line in wf_lines) {
  name <- sub("\\{.*", "", line)
  rules_str <- sub(".*\\{(.*)\\}", "\\1", line)
  wfs[[name]] <- strsplit(rules_str, ",")[[1]]
}

count_combinations <- function(target, ranges) {
  if (target == "R") return(0)
  if (target == "A") return(prod(sapply(ranges, function(r) max(0, r[2] - r[1] + 1))))
  
  rules <- wfs[[target]]
  total <- 0
  curr_ranges <- ranges
  
  for (rule in rules) {
    if (!grepl(":", rule)) {
      total <- total + count_combinations(rule, curr_ranges)
      break
    }
    
    parts <- strsplit(rule, ":")[[1]]
    cond <- parts[1]
    dest <- parts[2]
    var <- substr(cond, 1, 1)
    op <- substr(cond, 2, 2)
    val <- as.numeric(substring(cond, 3))
    
    r <- curr_ranges[[var]]
    true_r <- r
    false_r <- r
    
    if (op == "<") {
      true_r[2] <- min(r[2], val - 1)
      false_r[1] <- max(r[1], val)
    } else {
      true_r[1] <- max(r[1], val + 1)
      false_r[2] <- min(r[2], val)
    }
    
    if (true_r[1] <= true_r[2]) {
      next_ranges <- curr_ranges
      next_ranges[[var]] <- true_r
      total <- total + count_combinations(dest, next_ranges)
    }
    
    if (false_r[1] <= false_r[2]) {
      curr_ranges[[var]] <- false_r
    } else {
      curr_ranges <- NULL
      break
    }
  }
  return(total)
}

initial_ranges <- list(x = c(1, 4000), m = c(1, 4000), a = c(1, 4000), s = c(1, 4000))
cat(count_combinations("in", initial_ranges), "\n")
