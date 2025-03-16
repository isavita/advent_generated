
# Function to calculate happiness for a given permutation
calculate_happiness <- function(perm, happiness_matrix) {
  n <- length(perm)
  total_happiness <- 0
  for (i in 1:n) {
    left_neighbor <- ifelse(i == 1, perm[n], perm[i - 1])
    right_neighbor <- ifelse(i == n, perm[1], perm[i + 1])
    total_happiness <- total_happiness + happiness_matrix[perm[i], left_neighbor] + happiness_matrix[perm[i], right_neighbor]
  }
  return(total_happiness)
}

# Function to generate all permutations (efficiently using recursion)
permute <- function(arr) {
  n <- length(arr)
  if (n == 0) {
    return(list())  # Return an empty list for an empty input.
  }
  if (n == 1) {
    return(list(arr)) # Return list with single element for single-element input
  }

  result <- list()
  for (i in 1:n) {
    rest <- arr[-i]
    sub_permutations <- permute(rest)
    for (sub_perm in sub_permutations) {
      result[[length(result) + 1]] <- c(arr[i], sub_perm) # prepend current element
    }
  }
  return(result)
}


main <- function() {
  # Read input from file
  lines <- readLines("input.txt")
  
  # Parse input and create a list of people and happiness changes
  happiness_data <- list()
  people <- c()
  for (line in lines) {
    parts <- unlist(strsplit(line, " "))
    person1 <- parts[1]
    gain_lose <- ifelse(parts[3] == "gain", 1, -1)
    amount <- as.integer(parts[4]) * gain_lose
    person2 <- gsub("\\.", "", parts[11]) # Remove trailing period
    
    if (!person1 %in% people) {
      people <- c(people, person1)
    }
    if(!person2 %in% people){
      people <- c(people, person2)
    }
    
    happiness_data[[paste(person1, person2, sep = "_")]] <- amount
  }
  
  # Create a happiness matrix
  n_people <- length(people)
  happiness_matrix <- matrix(0, nrow = n_people, ncol = n_people, dimnames = list(people, people))
  for (i in 1:n_people){
    for (j in 1:n_people){
        key1 <- paste(people[i], people[j], sep = "_")
        key2 <- paste(people[j], people[i], sep = "_")
        if(key1 %in% names(happiness_data)){
          happiness_matrix[i,j] <- happiness_data[[key1]]
        }
    }
  }

  # Part 1: Find optimal seating arrangement
  
  permutations <- permute(people)
  max_happiness <- -Inf
  
  for (perm in permutations) {
    current_happiness <- calculate_happiness(perm, happiness_matrix)
    max_happiness <- max(max_happiness, current_happiness)
  }
  
  cat("Part 1:", max_happiness, "\n")
  
  # Part 2: Add myself and recalculate
  
  myself <- "Me"
  people_with_me <- c(people, myself)
  n_people_with_me <- length(people_with_me)
  happiness_matrix_with_me <- matrix(0, nrow = n_people_with_me, ncol = n_people_with_me, dimnames = list(people_with_me, people_with_me))
    
  for (i in 1:n_people){
      for(j in 1:n_people){
          happiness_matrix_with_me[i, j] <- happiness_matrix[i,j]
      }
  }

  permutations_with_me <- permute(people_with_me)
  max_happiness_with_me <- -Inf
  
  for (perm in permutations_with_me) {
    current_happiness <- calculate_happiness(perm, happiness_matrix_with_me)
    max_happiness_with_me <- max(max_happiness_with_me, current_happiness)
  }
  
  cat("Part 2:", max_happiness_with_me, "\n")
}

main()
