
# Function to calculate quantum entanglement (product of weights)
quantum_entanglement <- function(group) {
  if (length(group) == 0) {
    return(1)  # Handle empty group case
  }
  return(prod(group))
}

# Function to check if a valid split exists for remaining weights
find_valid_split <- function(remaining_weights, target_weight) {
  n <- length(remaining_weights)
  if (n == 0) {
      return(target_weight == 0)
  }

    for (i in 1: (2^n -1)){
        group2_indices <- which(as.logical(intToBits(i)[1:n]))
        group2 <- remaining_weights[group2_indices]
        if(sum(group2) == target_weight){
            group3 <- remaining_weights[-group2_indices]
            if(sum(group3) == target_weight){
                return (TRUE)
            }
        }
    }
  return(FALSE)
}


solve <- function(weights) {
  total_weight <- sum(weights)
  target_weight <- total_weight / 3

  if (total_weight %% 3 != 0) {
    return(NULL) # No solution if weights can't be divided equally
  }

  best_qe <- Inf
  min_packages <- Inf

  for (num_packages in 1:length(weights)) {
    combinations_list <- combn(weights, num_packages, simplify = FALSE)

    for (group1 in combinations_list) {
      if (sum(group1) == target_weight) {
        remaining_weights <- weights[!(weights %in% group1) | duplicated(weights) ]
        if(find_valid_split(remaining_weights, target_weight)){

          qe <- quantum_entanglement(group1)

          if (num_packages < min_packages) {
            min_packages <- num_packages
            best_qe <- qe
          } else if (num_packages == min_packages && qe < best_qe) {
            best_qe <- qe
          }
        }
        }
      }
    if(min_packages != Inf){
      #if we found solution for current group size we do not need to check larger groups
      break;
    }
  }

  return(best_qe)
}



main <- function() {
  # Read input from file
  weights <- scan("input.txt")
  
  #Solve
  result <- solve(weights)

  # Print the result
  cat(result, "\n")
}

# Set main as the entry point
main()
