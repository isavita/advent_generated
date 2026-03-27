
library(digest)

input <- readLines("input.txt", warn = FALSE)
passcode <- if (length(input) > 0) trimws(input[1]) else ""

longest <- 0
stack <- list(list(x = 0, y = 0, p = ""))

while (length(stack) > 0) {
  curr <- stack[[length(stack)]]
  stack[[length(stack)]] <- NULL
  
  if (curr$x == 3 && curr$y == 3) {
    len <- nchar(curr$p)
    if (len > longest) longest <- len
    next
  }
  
  h <- digest(paste0(passcode, curr$p), algo = "md5", serialize = FALSE)
  open <- charToRaw(h)[1:4] >= 98
  
  if (open[4] && curr$x < 3) {
    stack[[length(stack) + 1]] <- list(x = curr$x + 1, y = curr$y, p = paste0(curr$p, "R"))
  }
  if (open[3] && curr$x > 0) {
    stack[[length(stack) + 1]] <- list(x = curr$x - 1, y = curr$y, p = paste0(curr$p, "L"))
  }
  if (open[2] && curr$y < 3) {
    stack[[length(stack) + 1]] <- list(x = curr$x, y = curr$y + 1, p = paste0(curr$p, "D"))
  }
  if (open[1] && curr$y > 0) {
    stack[[length(stack) + 1]] <- list(x = curr$x, y = curr$y - 1, p = paste0(curr$p, "U"))
  }
}

cat(longest, "\n")
