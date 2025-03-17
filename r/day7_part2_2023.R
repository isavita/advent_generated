
# Function to determine the type of a hand
get_hand_type <- function(hand, part2 = FALSE) {
  counts <- table(strsplit(hand, "")[[1]])
  
  if (part2 && "J" %in% names(counts)) {
    joker_count <- counts["J"]
    counts <- counts[names(counts) != "J"]
    if (length(counts) == 0) {
      return(7) # Five of a kind (all jokers)
    }
    max_count_idx <- which.max(counts)
    counts[max_count_idx] <- counts[max_count_idx] + joker_count
  }
  
  counts <- sort(counts, decreasing = TRUE)
  
  if (counts[1] == 5) {
    return(7) # Five of a kind
  } else if (counts[1] == 4) {
    return(6) # Four of a kind
  } else if (counts[1] == 3 && length(counts) > 1 && counts[2] == 2) {
    return(5) # Full house
  } else if (counts[1] == 3) {
    return(4) # Three of a kind
  } else if (counts[1] == 2 && length(counts) > 1 && counts[2] == 2) {
    return(3) # Two pair
  } else if (counts[1] == 2) {
    return(2) # One pair
  } else {
    return(1) # High card
  }
}

# Function to compare two hands
compare_hands <- function(hand1, hand2, part2 = FALSE) {
  card_ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
  if(part2){
      card_ranks <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")
  }
  
  type1 <- get_hand_type(hand1, part2)
  type2 <- get_hand_type(hand2, part2)
  
  if (type1 != type2) {
    return(type1 - type2)
  } else {
    hand1_cards <- strsplit(hand1, "")[[1]]
    hand2_cards <- strsplit(hand2, "")[[1]]
    for (i in 1:5) {
      rank1 <- which(card_ranks == hand1_cards[i])
      rank2 <- which(card_ranks == hand2_cards[i])
      if (rank1 != rank2) {
        return(rank1 - rank2)
      }
    }
    return(0)
  }
}

# Function to calculate total winnings
calculate_winnings <- function(hands_bids, part2 = FALSE) {
  # Sort hands based on custom comparison function
    if(part2){
        sorted_indices <- order(sapply(hands_bids$hand, function(x) get_hand_type(x, TRUE)),
                                sapply(1:nrow(hands_bids), function(i) {
                                  paste(sapply(strsplit(hands_bids$hand[i], "")[[1]], function(card) {
                                    sprintf("%02d", which(c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A") == card))
                                  }), collapse = "")
                                }))
    } else {
        sorted_indices <- order(sapply(hands_bids$hand, function(x) get_hand_type(x, FALSE)),
                              sapply(1:nrow(hands_bids), function(i) {
                                paste(sapply(strsplit(hands_bids$hand[i], "")[[1]], function(card) {
                                  sprintf("%02d", which(c("2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A") == card))
                                }), collapse = "")
                              }))
    }
    
  sorted_hands_bids <- hands_bids[sorted_indices, ]
  total_winnings <- sum(sorted_hands_bids$bid * (1:nrow(sorted_hands_bids)))
  return(total_winnings)
}


# Main function
main <- function() {
  # Read input from file
  input_lines <- readLines("input.txt")
  
  # Parse hands and bids
  hands_bids <- data.frame(
    hand = character(),
    bid = integer(),
    stringsAsFactors = FALSE
  )
  for (line in input_lines) {
    parts <- strsplit(line, " ")[[1]]
    hands_bids <- rbind(hands_bids, data.frame(hand = parts[1], bid = as.integer(parts[2]), stringsAsFactors = FALSE))
  }
  
  # Calculate total winnings for Part 1
  total_winnings_part1 <- calculate_winnings(hands_bids, part2 = FALSE)
  cat("Total winnings (Part 1):", total_winnings_part1, "\n")
  
  # Calculate total winnings for Part 2
  total_winnings_part2 <- calculate_winnings(hands_bids, part2 = TRUE)
  cat("Total winnings (Part 2):", total_winnings_part2, "\n")
}

# Run the main function
main()
