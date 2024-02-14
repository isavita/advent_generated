
# Read input from file
input <- readLines("input.txt")

# Function to check if one range fully contains the other
contains <- function(x, y) {
  x_range <- as.numeric(unlist(strsplit(x, "-")))
  y_range <- as.numeric(unlist(strsplit(y, "-")))
  
  if (x_range[1] <= y_range[1] && x_range[2] >= y_range[2]) {
    return(TRUE)
  } else if (y_range[1] <= x_range[1] && y_range[2] >= x_range[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Count number of assignment pairs where one range fully contains the other
count <- 0
for (pair in input) {
  sections <- unlist(strsplit(pair, ","))
  if (contains(sections[1], sections[2])) {
    count <- count + 1
  }
}

print(count)
