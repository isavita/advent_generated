
# Function to perform replacements and count distinct molecules
count_distinct_molecules <- function(replacements, molecule) {
  distinct_molecules <- character(0)
  
  for (replacement in names(replacements)) {
    for (to_replace in replacements[[replacement]]) {
      positions <- gregexpr(replacement, molecule)[[1]]
      if (positions[1] != -1) {
        for (pos in positions) {
          new_molecule <- paste0(
            substr(molecule, 1, pos - 1),
            to_replace,
            substr(molecule, pos + nchar(replacement), nchar(molecule))
          )
          if (!new_molecule %in% distinct_molecules) {
            distinct_molecules <- c(distinct_molecules, new_molecule)
          }
        }
      }
    }
  }
  return(length(distinct_molecules))
}

# Read input from file
input <- readLines("input.txt")

# Separate replacements and molecule
separator_index <- which(input == "")
replacements_input <- input[1:(separator_index - 1)]
molecule <- input[separator_index + 1]

# Parse replacements
replacements <- list()
for (line in replacements_input) {
  parts <- strsplit(line, " => ")[[1]]
  from <- parts[1]
  to <- parts[2]
  if (from %in% names(replacements)) {
    replacements[[from]] <- c(replacements[[from]], to)
  } else {
    replacements[[from]] <- to
  }
}

# Calculate and print the result
result <- count_distinct_molecules(replacements, molecule)
cat(result, "\n")
