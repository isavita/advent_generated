
# Read input from file
input <- readLines("input.txt")

# Function to calculate priority of item type
priority <- function(item){
  if(item %in% letters){
    return(match(item, letters))
  } else {
    return(match(item, LETTERS) + 26)
  }
}

# Calculate sum of priorities for each rucksack
total <- 0
for (rucksack in input){
  items <- strsplit(rucksack, "")[[1]]
  common_item <- intersect(items[1:(length(items)/2)], items[(length(items)/2 + 1):length(items)])
  total <- total + priority(common_item)
}

# Print the sum of priorities
print(total)
