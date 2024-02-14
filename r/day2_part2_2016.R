
# Read input from file
instructions <- readLines("input.txt")

# Define keypad
keypad <- list(
  "1" = c(D = "3"),
  "2" = c(R = "3", D = "6"),
  "3" = c(U = "1", R = "4", D = "7", L = "2"),
  "4" = c(L = "3", D = "8"),
  "5" = c(R = "6"),
  "6" = c(U = "2", R = "7", D = "A", L = "5"),
  "7" = c(U = "3", R = "8", D = "B", L = "6"),
  "8" = c(U = "4", R = "9", D = "C", L = "7"),
  "9" = c(L = "8"),
  "A" = c(U = "6", R = "B"),
  "B" = c(U = "7", R = "C", D = "D", L = "A"),
  "C" = c(U = "8", L = "B"),
  "D" = c(U = "B")
)

# Initialize starting position and code
position <- "5"
code <- ""

# Iterate through instructions to determine code
for (instruction in instructions) {
  for (move in strsplit(instruction, "")[[1]]) {
    if (move %in% names(keypad[[position]])) {
      position <- keypad[[position]][[move]]
    }
  }
  code <- paste0(code, position)
}

# Print the final code
cat(code, "\n")
