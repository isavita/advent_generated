
# Step 1: Read Input
input <- readLines("input.txt")

# Step 2: Initialize Variables
score <- 0
depth <- 0
inGarbage <- FALSE
cancelNext <- FALSE

# Step 3: Process Stream
for (line in input) {
  for (ch in strsplit(line, "")[[1]]) {
    if (cancelNext) {
      cancelNext <- FALSE
      next
    }
    
    if (inGarbage) {
      if (ch == '!') {
        cancelNext <- TRUE
      } else if (ch == '>') {
        inGarbage <- FALSE
      }
    } else {
      if (ch == '{') {
        depth <- depth + 1
      } else if (ch == '}') {
        score <- score + depth
        depth <- depth - 1
      } else if (ch == '<') {
        inGarbage <- TRUE
      }
    }
  }
}

# Step 4: Print Score
cat(score, "\n")
