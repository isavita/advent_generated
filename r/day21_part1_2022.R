
lines <- readLines("input.txt")
jobs <- new.env()
memo <- new.env()

for (line in lines) {
  parts <- strsplit(line, ": ")[[1]]
  jobs[[parts[1]]] <- parts[2]
}

calculate <- function(name) {
  if (exists(name, envir = memo)) {
    return(memo[[name]])
  }
  
  job <- jobs[[name]]
  num_val <- suppressWarnings(as.numeric(job))
  
  if (!is.na(num_val)) {
    memo[[name]] <- num_val
    return(num_val)
  }
  
  tokens <- strsplit(job, " ")[[1]]
  left <- calculate(tokens[1])
  op <- tokens[2]
  right <- calculate(tokens[3])
  
  res <- switch(op,
    "+" = left + right,
    "-" = left - right,
    "*" = left * right,
    "/" = left / right
  )
  
  memo[[name]] <- res
  return(res)
}

result <- calculate("root")
cat(sprintf("%.0f\n", result))
