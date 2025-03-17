
# Function to calculate the number of matches for a single card
calculate_matches <- function(winning_numbers, my_numbers) {
  length(intersect(winning_numbers, my_numbers))
}

# Function to calculate points for a single card (Part 1)
calculate_points <- function(matches) {
  if (matches > 0) {
    return(2^(matches - 1))
  } else {
    return(0)
  }
}

# Main function
main <- function() {
  # Read the input file
  input <- readLines("input.txt")

  total_points <- 0
  card_counts <- rep(1, length(input))  # Initialize with one of each card

  for (i in seq_along(input)) {
    card <- input[i]
    card_parts <- strsplit(card, ": ")[[1]]
    numbers <- strsplit(card_parts[2], " \\| ")[[1]]
    
    winning_numbers <- as.integer(strsplit(numbers[1], "\\s+")[[1]])
    my_numbers <- as.integer(strsplit(numbers[2], "\\s+")[[1]])
    
    # Remove NA values, which can appear due to multiple spaces
    winning_numbers <- winning_numbers[!is.na(winning_numbers)]
    my_numbers <- my_numbers[!is.na(my_numbers)]


    matches <- calculate_matches(winning_numbers, my_numbers)

    # Part 1: Calculate total points
    total_points <- total_points + calculate_points(matches)

    # Part 2: Update card counts
    if (matches > 0) {
      for (j in (i + 1):min(i + matches, length(input))) {
        card_counts[j] <- card_counts[j] + card_counts[i]
      }
    }
  }

  # Print results
  cat("Part 1:", total_points, "\n")
  cat("Part 2:", sum(card_counts), "\n")
}

# Run the main function
if (interactive()) {
    # Create a dummy input.txt for interactive testing
    writeLines(c(
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ), "input.txt")
  main()
} else {
    # Create input.txt file
    if (!file.exists("input.txt")) {
      cat("Error: input.txt not found.\n")
    } else{
        main()
    }
}
