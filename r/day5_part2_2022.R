
move <- function(st, steps) {
  stacks <- lapply(st, function(x) rev(x))
  
  for (step in steps) {
    parts <- strsplit(step, " ")[[1]]
    n <- as.integer(parts[2])
    frm <- as.integer(parts[4])
    to <- as.integer(parts[6])
    
    
    stacks[[to]] <- c(stacks[[to]], tail(stacks[[frm]], n))
    stacks[[frm]] <- head(stacks[[frm]], length(stacks[[frm]]) - n)
  }
  
  paste(sapply(stacks, function(x) tail(x, 1)), collapse = "")
}

main <- function() {
  data <- readLines("input.txt")
  sep_index <- which(data == "")
  
  input_data <- data[1:(sep_index - 1)]
  num_stacks <- (nchar(input_data[1]) + 1) / 4
  stacks <- lapply(1:num_stacks, function(x) character(0))
  
  for (line in input_data) {
    for (i in 1:nchar(line)) {
      b <- substr(line, i, i)
      if (b >= 'A' && b <= 'Z') {
        stacks[[(i - 1) / 4 + 1]] <- c(stacks[[(i - 1) / 4 + 1]], b)
      }
    }
  }
  
  steps <- data[(sep_index + 1):length(data)]
    steps <- steps[steps != ""]
  
  cat(move(stacks, steps))
}

main()
