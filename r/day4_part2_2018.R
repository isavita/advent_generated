
# Function to parse the input and extract relevant information
parse_input <- function(file_path) {
    lines <- readLines(file_path)
    # Sort the lines chronologically
    lines <- sort(lines)
    
    records <- data.frame(
        timestamp = character(length(lines)),
        event = character(length(lines)),
        stringsAsFactors = FALSE
    )
    
    for (i in seq_along(lines)) {
        records$timestamp[i] <- substr(lines[i], 2, 17)
        records$event[i] <- substr(lines[i], 19, nchar(lines[i]))
    }
    
    return(records)
}

# Function to process the records and calculate sleep times
process_records <- function(records) {
    guard_sleep_minutes <- new.env(hash = TRUE)
    current_guard <- NA
    sleep_start <- NA
    
    for (i in 1:nrow(records)) {
        if (grepl("Guard", records$event[i])) {
            current_guard <- as.integer(gsub("[^0-9]", "", records$event[i]))
            sleep_start <- NA
        } else if (grepl("falls asleep", records$event[i])) {
            sleep_start <- as.integer(substr(records$timestamp[i], 15, 16))
        } else if (grepl("wakes up", records$event[i])) {
            wake_up <- as.integer(substr(records$timestamp[i], 15, 16))
            if (!is.na(current_guard) && !is.na(sleep_start)) {
                for (minute in sleep_start:(wake_up - 1)) {
                    key <- paste(current_guard, minute, sep = "_")
                    if (exists(key, envir = guard_sleep_minutes)) {
                      guard_sleep_minutes[[key]] <- guard_sleep_minutes[[key]] + 1
                    } else {
                      guard_sleep_minutes[[key]] <- 1
                    }
                }
            }
            sleep_start <- NA
        }
    }

    # Convert environment to list for easier processing in the next steps
    sleep_list <- as.list(guard_sleep_minutes)
    
    return(sleep_list)
}

# Strategy 1: Find the guard with the most minutes asleep
strategy1 <- function(sleep_list) {
    guard_totals <- new.env(hash = TRUE)
  
    for(key_str in names(sleep_list)){
        key_parts <- strsplit(key_str, "_")[[1]]
        guard_id <- as.integer(key_parts[1])

        if (exists(as.character(guard_id), envir = guard_totals)) {
                guard_totals[[as.character(guard_id)]] <- guard_totals[[as.character(guard_id)]] + sleep_list[[key_str]]
              } else {
                guard_totals[[as.character(guard_id)]] <- sleep_list[[key_str]]
              }
      
    }

    guard_totals_list <- as.list(guard_totals)
  
    max_sleep <- 0
    max_guard <- NA
    for (guard in names(guard_totals_list)) {
        if (guard_totals_list[[guard]] > max_sleep) {
            max_sleep <- guard_totals_list[[guard]]
            max_guard <- as.integer(guard)
        }
    }
  
    best_minute <- NA
    max_minute_count <- 0
      for(key_str in names(sleep_list)){
        key_parts <- strsplit(key_str, "_")[[1]]
        guard_id <- as.integer(key_parts[1])
        minute <- as.integer(key_parts[2])
        if (guard_id == max_guard)
        {
          if (sleep_list[[key_str]] > max_minute_count)
          {
            max_minute_count <- sleep_list[[key_str]]
            best_minute <- minute
          }
        }
    }
  
    return(max_guard * best_minute)
}

# Strategy 2: Find the guard most frequently asleep on the same minute
strategy2 <- function(sleep_list) {

    best_guard <- NA
    best_minute <- NA
    max_count <- 0
    
    for(key_str in names(sleep_list)){
      key_parts <- strsplit(key_str, "_")[[1]]
      guard_id <- as.integer(key_parts[1])
      minute <- as.integer(key_parts[2])
      
      if(sleep_list[[key_str]] > max_count)
      {
        max_count <- sleep_list[[key_str]]
        best_guard <- guard_id
        best_minute <- minute
      }
    }    
    return(best_guard * best_minute)
}

# Main function
main <- function() {
    # Parse the input
    records <- parse_input("input.txt")
    
    # Process records to calculate sleep times
    sleep_data <- process_records(records)
    
    # Strategy 1
    result1 <- strategy1(sleep_data)
    cat("Strategy 1 Result:", result1, "\n")
    
    # Strategy 2
    result2 <- strategy2(sleep_data)
    cat("Strategy 2 Result:", result2, "\n")
}

# Run the main function
main()
