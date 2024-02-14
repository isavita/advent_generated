
# Read input from file
input <- readLines("input.txt")

# Function to calculate priority of item type
calculate_priority <- function(item){
  if(item %in% letters){
    return(match(item, letters))
  } else {
    return(match(item, LETTERS) + 26)
  }
}

# Part One
part_one_sum <- 0
for (i in seq(1, length(input), by = 3)){
  rucksack1 <- strsplit(input[i], "")[[1]]
  rucksack2 <- strsplit(input[i+1], "")[[1]]
  
  common_items <- intersect(rucksack1, rucksack2)
  part_one_sum <- part_one_sum + sum(sapply(common_items, calculate_priority))
}

print(part_one_sum)

# Part Two
part_two_sum <- 0
for (i in seq(1, length(input), by = 3)){
  rucksack1 <- strsplit(input[i], "")[[1]]
  rucksack2 <- strsplit(input[i+1], "")[[1]]
  rucksack3 <- strsplit(input[i+2], "")[[1]]
  
  common_items <- Reduce(intersect, list(rucksack1, rucksack2, rucksack3))
  part_two_sum <- part_two_sum + sum(sapply(common_items, calculate_priority))
}

print(part_two_sum)
