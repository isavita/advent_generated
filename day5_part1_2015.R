
data <- readLines("input.txt")

nice_strings <- 0

for (string in data) {
  if (grepl("[aeiou].*[aeiou].*[aeiou]", string) && grepl("(.)\\1", string) && !grepl("ab|cd|pq|xy", string)) {
    nice_strings <- nice_strings + 1
  }
}

print(nice_strings)
