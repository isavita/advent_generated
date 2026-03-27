
lines <- readLines("input.txt", warn = FALSE)
blanks <- which(lines == "")

rule_lines <- lines[1:(blanks[1]-1)]
re <- "^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$"
matches <- regmatches(rule_lines, regexec(re, rule_lines))
rules <- lapply(matches, function(m) {
  list(name = m[2], l1 = as.numeric(m[3]), h1 = as.numeric(m[4]), 
       l2 = as.numeric(m[5]), h2 = as.numeric(m[6]))
})

my_ticket <- as.numeric(strsplit(lines[blanks[1]+2], ",")[[1]])

nearby_lines <- lines[(blanks[2]+2):length(lines)]
nearby_lines <- nearby_lines[nearby_lines != ""]
nearby <- do.call(rbind, lapply(strsplit(nearby_lines, ","), as.numeric))

is_v <- matrix(FALSE, nrow(nearby), ncol(nearby))
for (r in rules) {
  is_v <- is_v | (nearby >= r$l1 & nearby <= r$h1) | (nearby >= r$l2 & nearby <= r$h2)
}
valid_mask <- rowSums(is_v) == ncol(is_v)
valid_tickets <- rbind(my_ticket, nearby[valid_mask, , drop = FALSE])

num_r <- length(rules)
num_f <- length(my_ticket)
M <- matrix(FALSE, num_r, num_f)
for (i in 1:num_r) {
  r <- rules[[i]]
  is_ok <- (valid_tickets >= r$l1 & valid_tickets <= r$h1) | (valid_tickets >= r$l2 & valid_tickets <= r$h2)
  M[i, ] <- colSums(is_ok) == nrow(valid_tickets)
}

assigned_cols <- numeric(num_r)
while (any(assigned_cols == 0)) {
  # Rules with only one possible column
  row_s <- rowSums(M)
  i <- which(row_s == 1)[1]
  if (!is.na(i)) {
    j <- which(M[i, ])
    assigned_cols[i] <- j
    M[, j] <- FALSE
    M[i, ] <- FALSE
  } else {
    # Columns with only one possible rule
    col_s <- colSums(M)
    j <- which(col_s == 1)[1]
    i <- which(M[, j])
    assigned_cols[i] <- j
    M[, j] <- FALSE
    M[i, ] <- FALSE
  }
}

departure_idx <- grep("^departure", sapply(rules, function(r) r$name))
result <- prod(as.numeric(my_ticket[assigned_cols[departure_idx]]))
cat(sprintf("%.0f\n", result))
