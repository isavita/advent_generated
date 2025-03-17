
# Function to simulate the marble game
simulate_marble_game <- function(num_players, last_marble) {
  scores <- rep(0, num_players)
  circle <- list(0)  # Use a list to represent the circular structure
  current_marble_index <- 1
  current_player <- 1

  for (marble in 1:last_marble) {
    if (marble %% 23 == 0) {
      scores[current_player] <- scores[current_player] + marble
      remove_index <- (current_marble_index - 7) %% length(circle)
      if(remove_index == 0) remove_index <- length(circle) # Adjust for R's 1-based indexing
      scores[current_player] <- scores[current_player] + circle[[remove_index]]
      circle <- circle[-remove_index]
      current_marble_index <- remove_index

    } else {
      insert_index <- (current_marble_index + 1) %% length(circle) +1
      
      # Efficient insertion using append and a bit of clever indexing
      if (insert_index > length(circle)) {
          circle <- append(circle, marble)  #append at the end
      }
      else
      {
        circle <- append(circle, marble, after = insert_index -1) #insert at the right index
      }
      
      current_marble_index <- insert_index
    }
    current_player <- (current_player %% num_players) + 1
  }
  return(max(scores))
}

# Main function to read input and print output
main <- function() {
  # Read input from input.txt
  input <- readLines("input.txt")
  
  # Parse the input string (assuming a consistent format)
  parts <- strsplit(input, " ")[[1]]
  num_players <- as.integer(parts[1])
  last_marble <- as.integer(parts[7])

  # Calculate and print the high score
  high_score <- simulate_marble_game(num_players, last_marble)
  cat(high_score, "\n")
}

# Run the main function
if (interactive()) {
    # Create a dummy input.txt for interactive testing
    writeLines("9 players; last marble is worth 25 points", "input.txt")
    main()  # Example with small values for quick testing
    writeLines("10 players; last marble is worth 1618 points", "input.txt")
    main()
    writeLines("13 players; last marble is worth 7999 points", "input.txt")
    main()
    writeLines("432 players; last marble is worth 71019 points", "input.txt")
    main()

} else {
  main()
}

