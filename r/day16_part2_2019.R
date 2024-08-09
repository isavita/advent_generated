input <- readLines("input.txt")[1]
repeated_input <- as.integer(unlist(strsplit(paste(rep(input, 10000), collapse = ""), "")))

offset <- as.integer(substr(input, 1, 7))
n <- length(repeated_input)

for (phase in 1:100) {
  sum <- 0
  for (i in n:offset) {
    sum <- sum + repeated_input[i]
    repeated_input[i] <- sum %% 10
  }
}

cat(repeated_input[(offset + 1):(offset + 8)], sep = "")
cat("\n")