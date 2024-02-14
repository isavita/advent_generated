
# Read input from file
input <- readLines("input.txt")

# Calculate total square feet of wrapping paper needed
total_paper <- sum(sapply(strsplit(input, "x"), function(x) {
  sides <- as.numeric(x)
  areas <- c(sides[1]*sides[2], sides[2]*sides[3], sides[3]*sides[1])
  2*sum(areas) + min(areas)
}))

# Print total square feet of wrapping paper needed
print(total_paper)

# Calculate total feet of ribbon needed
total_ribbon <- sum(sapply(strsplit(input, "x"), function(x) {
  sides <- sort(as.numeric(x))
  ribbon_wrap <- 2*(sides[1] + sides[2])
  ribbon_bow <- prod(sides)
  ribbon_wrap + ribbon_bow
}))

# Print total feet of ribbon needed
print(total_ribbon)
