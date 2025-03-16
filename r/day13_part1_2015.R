
# Function to calculate happiness change for a given seating arrangement
calculate_happiness <- function(arrangement, happiness_matrix) {
    n <- length(arrangement)
    total_happiness <- 0
    for (i in 1:n) {
        left_neighbor <- ifelse(i == 1, arrangement[n], arrangement[i - 1])
        right_neighbor <- ifelse(i == n, arrangement[1], arrangement[i + 1])
        total_happiness <- total_happiness + happiness_matrix[arrangement[i], left_neighbor] + happiness_matrix[arrangement[i], right_neighbor]
    }
    return(total_happiness)
}

# Function to generate all permutations of a vector
permute <- function(v) {
    n <- length(v)
    if (n == 1) {
        return(list(v))
    } else {
        res <- list()
        for (i in 1:n) {
            sub_permutations <- permute(v[-i])
            for (sub_permutation in sub_permutations) {
                res[[length(res) + 1]] <- c(v[i], sub_permutation)
            }
        }
        return(res)
    }
}

# Main function
main <- function() {
    # Read input from file
    lines <- readLines("input.txt")
    
    # Create a list to store the happiness changes
    happiness_changes <- list()
    
    # Parse the input lines
    for (line in lines) {
        parts <- strsplit(line, " ")[[1]]
        person1 <- parts[1]
        gain_lose <- ifelse(parts[3] == "gain", 1, -1)
        happiness <- as.integer(parts[4]) * gain_lose
        person2 <- substr(parts[11], 1, nchar(parts[11]) - 1) # Remove the trailing "."
        
        if (!person1 %in% names(happiness_changes)) {
          happiness_changes[[person1]] <- list()
        }
      
        happiness_changes[[person1]][[person2]] <- happiness
    }

    # Get the unique list of people
    people <- names(happiness_changes)
    
    # Create a matrix to look up happiness values efficiently
    happiness_matrix <- matrix(0, nrow=length(people), ncol=length(people), dimnames=list(people, people))
    for(p1 in people) {
      for(p2 in names(happiness_changes[[p1]])){
        happiness_matrix[p1,p2] <- happiness_changes[[p1]][[p2]]
      }
    }

    # Generate all possible seating arrangements (permutations)
    arrangements <- permute(people)
    
    # Calculate happiness for each arrangement and find the maximum
    max_happiness <- -Inf
    for (arrangement in arrangements) {
        current_happiness <- calculate_happiness(arrangement, happiness_matrix)
        max_happiness <- max(max_happiness, current_happiness)
    }
    
    # Print the result
    cat(max_happiness, "\n")
}

# Run the main function
main()
