react <- function(polymer) {
  stack <- character()
  
  for (unit in strsplit(polymer, "")[[1]]) {
    if (length(stack) > 0 && tolower(stack[length(stack)]) == tolower(unit) && 
        stack[length(stack)] != unit) {
      stack <- stack[-length(stack)]
    } else {
      stack <- c(stack, unit)
    }
  }
  
  paste(stack, collapse = "")
}

polymer <- trimws(readLines("input.txt"))
min_length <- nchar(polymer)

for (unit in letters) {
  temp_polymer <- gsub(paste0("[", unit, toupper(unit), "]"), "", polymer)
  reacted_polymer <- react(temp_polymer)
  min_length <- min(min_length, nchar(reacted_polymer))
}

cat(min_length, "\n")