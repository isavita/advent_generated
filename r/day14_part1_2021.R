# Read input from the file
input <- readLines("input.txt")
template <- input[1]
rules <- strsplit(input[-1], " -> ")
rule_list <- setNames(sapply(rules, `[`, 2), sapply(rules, `[`, 1))

# Function to perform the polymerization process
polymerize <- function(template, rules, steps) {
  for (i in seq_len(steps)) {
    new_polymer <- character()
    for (j in seq_len(nchar(template) - 1)) {
      pair <- substring(template, j, j + 1)
      new_polymer <- c(new_polymer, substring(template, j, j), rules[pair])
    }
    new_polymer <- c(new_polymer, substring(template, nchar(template)))
    template <- paste(new_polymer, collapse = "")
  }
  template
}

# Perform 10 steps of polymerization
final_polymer <- polymerize(template, rule_list, 10)

# Count occurrences of each element
element_counts <- table(strsplit(final_polymer, "")[[1]])
most_common <- max(element_counts)
least_common <- min(element_counts)

# Calculate the difference
result <- most_common - least_common
cat(result, "\n")