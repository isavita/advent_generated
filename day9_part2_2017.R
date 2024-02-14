
input <- readLines("input.txt")
stream <- strsplit(input, "")[[1]]

score <- 0
total_score <- 0
garbage_count <- 0
ignore_next <- FALSE
in_garbage <- FALSE

for (char in stream) {
  if (ignore_next) {
    ignore_next <- FALSE
  } else if (char == "!") {
    ignore_next <- TRUE
  } else if (in_garbage) {
    if (char == ">") {
      in_garbage <- FALSE
    } else {
      garbage_count <- garbage_count + 1
    }
  } else {
    if (char == "{") {
      score <- score + 1
      total_score <- total_score + score
    } else if (char == "}") {
      score <- score - 1
    } else if (char == "<") {
      in_garbage <- TRUE
    }
  }
}

print(total_score)
print(garbage_count)
