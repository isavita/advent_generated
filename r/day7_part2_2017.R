programs <- list()

lines <- readLines("input.txt")
for (line in lines) {
  parts <- unlist(strsplit(line, " "))
  name <- parts[1]
  weight <- as.integer(gsub("[()]", "", parts[2]))
  holds <- if (length(parts) > 3) unlist(strsplit(gsub(",", "", parts[4:length(parts)]), " ")) else character(0)
  programs[[name]] <- list(weight = weight, holds = holds)
}

dfs <- function(name) {
  program <- programs[[name]]
  totalWeight <- program$weight
  weights <- integer()
  
  for (child in program$holds) {
    weight <- dfs(child)
    totalWeight <- totalWeight + weight
    weights <- c(weights, weight)
  }
  
  if (length(unique(weights)) > 1) {
    weightCounts <- table(weights)
    unbalancedWeight <- names(weightCounts[weightCounts == 1])
    correctWeight <- as.numeric(names(weightCounts[weightCounts != 1]))
    unbalancedProgram <- program$holds[which(weights == as.numeric(unbalancedWeight))]
    print(programs[[unbalancedProgram]]$weight + (correctWeight - as.numeric(unbalancedWeight)))
    return(0)
  }
  
  return(totalWeight)
}

root <- "dtacyn" # Replace this with the root found in Part One
dfs(root)