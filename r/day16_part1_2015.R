# Define the known quantities of items that the real Sue has
mfcsam <- c(
  children = 3, cats = 7, samoyeds = 2, pomeranians = 3,
  akitas = 0, vizslas = 0, goldfish = 5, trees = 3,
  cars = 2, perfumes = 1
)

# Read the input file
input <- readLines("input.txt")

# Loop through each line in the input
for (line in input) {
  # Split the line into parts
  parts <- strsplit(line, " ")[[1]]
  
  # Extract the Sue number
  sueNumber <- parts[2]
  
  # Initialize a flag to indicate if this Sue matches
  matches <- TRUE
  
  # Loop through each item in the line (starting from the third part)
  for (i in seq(3, length(parts), 2)) {
    # Extract the item name and count
    item <- gsub(":", "", parts[i])  # Remove the colon from the item name
    count <- as.integer(gsub("[^0-9]", "", parts[i + 1]))
    
    # Check if the item count matches the known quantity
    if (!(item %in% names(mfcsam))) {
      # If the item is not in the known quantities, skip it
      next
    } else if (mfcsam[item] != count) {
      # If the counts don't match, set the flag to FALSE and break the loop
      matches <- FALSE
      break
    }
  }
  
  # If this Sue matches, print the Sue number and break the loop
  if (matches) {
    print(sueNumber)
    break
  }
}