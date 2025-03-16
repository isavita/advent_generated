
# Function to calculate the strength of a bridge
calculate_strength <- function(bridge) {
    strength <- 0
    for (i in seq_len(nrow(bridge))) {
        strength <- strength + sum(bridge[i, ])
    }
    return(strength)
}

# Recursive function to find the strongest and longest bridges
find_bridges <- function(components, current_bridge = matrix(nrow = 0, ncol = 2), current_port = 0) {
    
    best_strength <- 0
    longest_length <- 0
    longest_strength <- 0

    
    available_indices <- which(components[, 1] == current_port | components[, 2] == current_port)

    if (length(available_indices) == 0) {
      
        current_strength <- calculate_strength(current_bridge)
        current_length <- nrow(current_bridge)

        return(list(strength = current_strength, length = current_length, longest_strength = current_strength))
    }

    for (i in available_indices) {
        
        next_port <- ifelse(components[i, 1] == current_port, components[i, 2], components[i, 1])
        new_bridge <- rbind(current_bridge, components[i, ])
        remaining_components <- components[-i, , drop = FALSE]

        result <- find_bridges(remaining_components, new_bridge, next_port)

        if (result$strength > best_strength) {
            best_strength <- result$strength
        }
        
        if (result$length > longest_length){
          longest_length <- result$length
          longest_strength <- result$longest_strength
        } else if (result$length == longest_length && result$longest_strength > longest_strength){
          longest_strength <- result$longest_strength
        }
    }

    return(list(strength = best_strength, length = longest_length, longest_strength = longest_strength))
}

# Main function
main <- function() {
    # Read input from file
    input_data <- readLines("input.txt")
    components <- matrix(nrow = length(input_data), ncol = 2)
    for (i in seq_along(input_data)) {
        parts <- as.integer(strsplit(input_data[i], "/")[[1]])
        components[i, ] <- parts
    }

    # Find the strongest bridge
    result <- find_bridges(components)

    # Print the results
    cat("Strength of the strongest bridge:", result$strength, "\n")
    cat("Strength of the longest bridge:", result$longest_strength, "\n")
}

# Run the main function
main()
