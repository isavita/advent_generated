
# Function to check if an update is in the correct order
is_correct_order <- function(update, rules) {
  for (i in 1:(length(update) - 1)) {
    for (j in (i + 1):length(update)) {
      rule_key <- paste(update[j], update[i], sep = "|")
      if (rule_key %in% names(rules)) {
        return(FALSE) # Found a violation
      }
    }
  }
  return(TRUE)
}

# Function to topologically sort an update based on rules (Kahn's Algorithm)
topo_sort <- function(update, rules) {
    
  in_degree <- integer(0)
  
  #Calculate in-degrees considering only nodes present in the current update
  for (page in update){
      in_degree[as.character(page)] <- 0
  }
  for (rule in names(rules)) {
      
    parts <- strsplit(rule, "\\|")[[1]]
    
    if(parts[1] %in% update & parts[2] %in% update){
        in_degree[parts[2]] <-  in_degree[parts[2]] + 1
    }
  }

  queue <- names(in_degree)[in_degree == 0]
  sorted_update <- character(0)

  while (length(queue) > 0) {
    u <- queue[1]
    queue <- queue[-1]
    sorted_update <- c(sorted_update, u)

      for (v in update) {
          rule_key <- paste(u,v,sep="|")
          if(rule_key %in% names(rules)){
              
              if(in_degree[as.character(v)] > 0){
                in_degree[as.character(v)] <- in_degree[as.character(v)] -1
                if (in_degree[as.character(v)] == 0) {
                  queue <- c(queue, as.character(v))
                }
              }
          }
      }
  }
    
  if(length(sorted_update) != length(update)){
      return(NULL) #Cycle detected, should not occur given problem constraints, but handled for completeness
  }

  return(sorted_update)
}


# Main function
main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  
  # Separate rules and updates
  rules_end_index <- which(input == "") -1
  rules_lines <- input[1:rules_end_index]
  updates_lines <- input[(rules_end_index + 2):length(input)]

  # Create a rules dictionary (hash map)
  rules <- list()
  for (rule in rules_lines) {
    rules[[rule]] <- TRUE
  }

  # Process updates
  correct_updates_middle_sum <- 0
  incorrect_updates_middle_sum <- 0

  for (update_line in updates_lines) {
    update <- strsplit(update_line, ",")[[1]]
    
    if (is_correct_order(update, rules)) {
      middle_index <- (length(update) + 1) %/% 2
      correct_updates_middle_sum <- correct_updates_middle_sum + as.integer(update[middle_index])
    } else{
        sorted_update <- topo_sort(update, rules)
        if(!is.null(sorted_update)){
          middle_index <- (length(sorted_update) + 1) %/% 2
          incorrect_updates_middle_sum <- incorrect_updates_middle_sum + as.integer(sorted_update[middle_index])
        }
    }
  }

  # Print results
  cat("Sum of middle page numbers of correctly-ordered updates:", correct_updates_middle_sum, "\n")
  cat("Sum of middle page numbers of incorrectly-ordered updates (after sorting):", incorrect_updates_middle_sum, "\n")
}

# Run the main function
main()
