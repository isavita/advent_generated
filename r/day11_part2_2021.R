
# Function to simulate a single step and count flashes
simulate_step <- function(octopuses) {
    rows <- nrow(octopuses)
    cols <- ncol(octopuses)
    flashes <- 0
    
    # Increase energy level of each octopus by 1
    octopuses <- octopuses + 1
    
    # Identify octopuses that will flash
    flashed <- matrix(FALSE, nrow = rows, ncol = cols)
    
    repeat {
        new_flashes <- FALSE
        for (i in 1:rows) {
            for (j in 1:cols) {
                if (octopuses[i, j] > 9 && !flashed[i, j]) {
                    flashes <- flashes + 1
                    flashed[i, j] <- TRUE
                    new_flashes <- TRUE
                    
                    # Increase energy level of adjacent octopuses
                    for (x in max(1, i - 1):min(rows, i + 1)) {
                        for (y in max(1, j - 1):min(cols, j + 1)) {
                            octopuses[x, y] <- octopuses[x, y] + 1
                        }
                    }
                }
            }
        }
        if (!new_flashes) break
    }
    
    # Reset energy level of flashed octopuses to 0
    octopuses[flashed] <- 0
    
    return(list(octopuses = octopuses, flashes = flashes))
}

# Main function
main <- function() {
    # Read input from file
    input_data <- readLines("input.txt")
    octopuses <- do.call(rbind, lapply(strsplit(input_data, ""), as.integer))
    
    # Part 1: Simulate 100 steps and count total flashes
    total_flashes <- 0
    octopuses_copy <- octopuses  # Work on a copy for Part 1
    for (step in 1:100) {
        result <- simulate_step(octopuses_copy)
        octopuses_copy <- result$octopuses
        total_flashes <- total_flashes + result$flashes
    }
    cat("Total flashes after 100 steps:", total_flashes, "\n")
    
    # Part 2: Find the first step during which all octopuses flash
    octopuses_copy <- octopuses # reset for part 2
    step <- 0
    repeat {
        step <- step + 1
        result <- simulate_step(octopuses_copy)
        octopuses_copy <- result$octopuses
        if (all(octopuses_copy == 0)) {
            cat("First step during which all octopuses flash:", step, "\n")
            break
        }
    }
}
# Run the main function
main()

