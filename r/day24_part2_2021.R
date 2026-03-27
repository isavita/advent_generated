
lines <- readLines("input.txt")
get_val <- function(idx) as.numeric(sapply(strsplit(lines[idx], " "), `[`, 3))

l <- get_val(seq(5, by = 18, length.out = 14))
k <- get_val(seq(6, by = 18, length.out = 14))
m <- get_val(seq(16, by = 18, length.out = 14))

stack <- c()
res <- numeric(14)
for (i in 1:14) {
  if (l[i] == 1) {
    stack <- c(stack, i)
  } else {
    pop <- tail(stack, 1)
    stack <- head(stack, -1)
    diff <- m[pop] + k[i]
    v_pop <- 1
    while (v_pop + diff < 1) v_pop <- v_pop + 1
    res[pop] <- v_pop
    res[i] <- v_pop + diff
  }
}

cat(paste(res, collapse = ""), "\n", sep = "")
