
options(scipen = 999)
input_lines <- readLines("input.txt")
mem <- new.env(hash = TRUE, parent = emptyenv())

for (line in input_lines) {
  if (startsWith(line, "mask")) {
    mask_vec <- strsplit(sub("mask = ", "", line), "")[[1]]
    xs <- which(mask_vec == "X")
    x_pows <- 2^(36 - xs)
    ones_mask <- which(mask_vec == "1")
  } else {
    parts <- regmatches(line, regexec("\\[(\\d+)\\] = (\\d+)", line))[[1]]
    addr <- as.numeric(parts[2])
    val <- as.numeric(parts[3])
    
    bits <- floor(addr / 2^(35:0)) %% 2
    bits[ones_mask] <- 1
    bits[xs] <- 0
    start_addr <- sum(bits * 2^(35:0))
    
    addrs <- start_addr
    for (p in x_pows) {
      addrs <- c(addrs, addrs + p)
    }
    
    for (a in addrs) {
      mem[[as.character(a)]] <- val
    }
  }
}

ans <- sum(unlist(as.list(mem), use.names = FALSE))
cat(format(ans, scientific = FALSE), "\n")
