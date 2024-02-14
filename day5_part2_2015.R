
# Part One
input <- readLines("input.txt")
nice_count <- 0
for (i in 1:length(input)) {
  if (grepl("[aeiou].*[aeiou].*[aeiou]", input[i]) && grepl("(.)\\1", input[i]) && !grepl("ab|cd|pq|xy", input[i])) {
    nice_count <- nice_count + 1
  }
}
print(nice_count)

# Part Two
nice_count_new <- 0
for (i in 1:length(input)) {
  if (grepl("([a-z][a-z]).*\\1", input[i]) && grepl("([a-z]).\\1", input[i])) {
    nice_count_new <- nice_count_new + 1
  }
}
print(nice_count_new)
