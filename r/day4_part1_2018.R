
# Function to parse the input and extract relevant information
parse_input <- function(file_path) {
  lines <- readLines(file_path)
  # Sort the lines chronologically
  lines <- sort(lines)
  
  records <- data.frame(timestamp = character(),
                        guard_id = integer(),
                        action = character(),
                        stringsAsFactors = FALSE)
  
  for (line in lines) {
    timestamp <- substr(line, 2, 17)
    action_str <- substr(line, 19, nchar(line))
    
    guard_id <- NA
    action <- NA
    
    if (grepl("Guard #", action_str)) {
      guard_id <- as.integer(gsub("[^0-9]", "", action_str))
      action <- "begins shift"
    } else if (grepl("falls asleep", action_str)) {
      action <- "falls asleep"
    } else if (grepl("wakes up", action_str)) {
      action <- "wakes up"
    }
    
    records <- rbind(records, data.frame(timestamp = timestamp,
                                         guard_id = guard_id,
                                         action = action,
                                         stringsAsFactors = FALSE))
  }
  
  return(records)
}

# Function to calculate sleep duration and most frequent sleep minute for each guard
analyze_sleep <- function(records) {
  guard_sleep_minutes <- list()
  current_guard <- NA
  sleep_start <- NA
  
  for (i in 1:nrow(records)) {
    if (!is.na(records$guard_id[i])) {
      current_guard <- records$guard_id[i]
    } else if (records$action[i] == "falls asleep") {
      sleep_start <- as.integer(substr(records$timestamp[i], 15, 16))
    } else if (records$action[i] == "wakes up") {
      wake_up <- as.integer(substr(records$timestamp[i], 15, 16))
      if (is.null(guard_sleep_minutes[[as.character(current_guard)]])) {
        guard_sleep_minutes[[as.character(current_guard)]] <- rep(0, 60)
      }
      guard_sleep_minutes[[as.character(current_guard)]][(sleep_start + 1):wake_up] <-
        guard_sleep_minutes[[as.character(current_guard)]][(sleep_start + 1):wake_up] + 1
    }
  }
  
  return(guard_sleep_minutes)
}

# Strategy 1: Find guard with most minutes asleep and their most frequent sleep minute
strategy1 <- function(guard_sleep_minutes) {
    
  total_sleep <- sapply(guard_sleep_minutes, sum)
  
  most_sleepy_guard <- names(total_sleep)[which.max(total_sleep)]
  most_sleepy_minute <- which.max(guard_sleep_minutes[[most_sleepy_guard]]) - 1
  
  return(as.integer(most_sleepy_guard) * most_sleepy_minute)
}

# Main function
main <- function() {
  # Parse the input file
  records <- parse_input("input.txt")
  
  # Analyze sleep patterns
  guard_sleep_minutes <- analyze_sleep(records)

  # Calculate the result using Strategy 1
  result <- strategy1(guard_sleep_minutes)
  
  # Print the result
  cat(result, "\n")
}

# Run the main function
main()
