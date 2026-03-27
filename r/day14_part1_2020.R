
options(scipen = 999)
lines <- readLines("input.txt")
mem <- new.env(hash = TRUE)
or_m <- 0
and_m <- 0

for (line in lines) {
  if (startsWith(line, "mask")) {
    m_str <- substr(line, 8, 43)
    or_m <- 0
    and_m <- 0
    for (i in 1:36) {
      char <- substr(m_str, i, i)
      p <- 2^(36 - i)
      if (char == "1") or_m <- or_m + p
      if (char == "X") and_m <- and_m + p
    }
  } else {
    parts <- as.numeric(regmatches(line, gregexpr("[0-9]+", line))[[1]])
    addr <- as.character(parts[1])
    val <- parts[2]
    
    v_h <- floor(val / 262144)
    v_l <- val %% 262144
    a_h <- floor(and_m / 262144)
    a_l <- and_m %% 262144
    
    new_val <- (bitwAnd(v_h, a_h) * 262144) + bitwAnd(v_l, a_l) + or_m
    mem[[addr]] <- new_val
  }
}

cat(sum(unlist(as.list(mem))), "\n")
