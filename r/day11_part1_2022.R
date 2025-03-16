
# Function to parse a monkey's data
parse_monkey <- function(monkey_data) {
  lines <- unlist(strsplit(monkey_data, "\n"))
  
  starting_items <- as.numeric(unlist(strsplit(gsub("[^0-9,]", "", lines[2]), ",")))
  
  operation_str <- gsub("  Operation: new = ", "", lines[3])
  operation <- function(old) {
    eval(parse(text=operation_str))
  }
  
  test_divisible <- as.numeric(gsub("[^0-9]", "", lines[4]))
  true_monkey <- as.numeric(gsub("[^0-9]", "", lines[5]))
  false_monkey <- as.numeric(gsub("[^0-9]", "", lines[6]))
  
  list(items = starting_items, 
       operation = operation, 
       test = function(x) x %% test_divisible == 0,
       true_monkey = true_monkey,
       false_monkey = false_monkey,
       inspected_count = 0)
}

# Main function
main <- function() {
  input_file <- "input.txt"
  
  # Read the entire file content
  file_content <- readChar(input_file, file.info(input_file)$size)
  
  # Split the content by double newlines to separate monkey data
  monkey_data_blocks <- unlist(strsplit(file_content, "\n\n"))
  
  # Parse each monkey's data
  monkeys <- lapply(monkey_data_blocks, parse_monkey)
  
  # Simulate rounds
  for (round in 1:20) {
    for (i in seq_along(monkeys)) {
      monkey <- monkeys[[i]]
      
      for (item in monkey$items) {
        # Inspect item
        new_worry <- floor(monkey$operation(item) / 3)
        monkeys[[i]]$inspected_count <- monkeys[[i]]$inspected_count + 1
        
        # Test and throw
        if (monkey$test(new_worry)) {
          target_monkey <- monkey$true_monkey + 1  # Adjust for 1-based indexing
        } else {
          target_monkey <- monkey$false_monkey + 1 # Adjust for 1-based indexing
        }
        
        monkeys[[target_monkey]]$items <- c(monkeys[[target_monkey]]$items, new_worry)
      }
      # Clear current monkey's items
      monkeys[[i]]$items <- numeric(0)
    }
  }
  
  # Calculate monkey business
  inspected_counts <- sapply(monkeys, function(x) x$inspected_count)
  sorted_counts <- sort(inspected_counts, decreasing = TRUE)
  monkey_business <- sorted_counts[1] * sorted_counts[2]
  
  cat(monkey_business, "\n")
}

main()
