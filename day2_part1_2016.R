
instructions <- readLines("input.txt")

keypad <- matrix(c(1:9), nrow = 3, byrow = TRUE)
current_button <- 5
code <- ""

for (instruction in instructions) {
  for (move in strsplit(instruction, "")[[1]]) {
    if (move == "U" && current_button > 3) current_button <- current_button - 3
    if (move == "D" && current_button < 7) current_button <- current_button + 3
    if (move == "L" && current_button %% 3 != 1) current_button <- current_button - 1
    if (move == "R" && current_button %% 3 != 0) current_button <- current_button + 1
  }
  code <- paste0(code, current_button)
}

print(code)
