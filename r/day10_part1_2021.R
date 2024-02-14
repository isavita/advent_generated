
input <- readLines("input.txt")

illegal_chars <- c(")", "]", "}", ">")
illegal_points <- c(3, 57, 1197, 25137)

total_score <- 0

for (line in input) {
  if (grepl("[\\[\\{\\(\\<]", line)) {
    stack <- c()
    for (char in strsplit(line, "")[[1]]) {
      if (char %in% c("(", "[", "{", "<")) {
        stack <- c(stack, char)
      } else if (char %in% c(")", "]", "}", ">")) {
        if (length(stack) == 0 || (char == ")" && stack[length(stack)] != "(") ||
            (char == "]" && stack[length(stack)] != "[") ||
            (char == "}" && stack[length(stack)] != "{") ||
            (char == ">" && stack[length(stack)] != "<")) {
          total_score <- total_score + illegal_points[which(illegal_chars == char)]
          break
        } else {
          stack <- stack[-length(stack)]
        }
      }
    }
  }
}

print(total_score)
