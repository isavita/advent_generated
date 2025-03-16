
# Function to parse rules and build a regex pattern
build_regex <- function(rules, rule_num = "0") {
  rule <- rules[[rule_num]]
  
  if (grepl('"', rule)) {
    return(gsub('"', '', rule))
  }
  
  regex_parts <- lapply(strsplit(rule, " \\| ")[[1]], function(part) {
    sub_rules <- strsplit(part, " ")[[1]]
    paste0(lapply(sub_rules, function(sub_rule) build_regex(rules, sub_rule)), collapse = "")
  })
  
  return(paste0("(", paste(regex_parts, collapse = "|"), ")"))
}


# Main function to process input and solve the puzzle
main <- function() {
  # Read input from file
  input <- readLines("input.txt")
  
  # Split rules and messages
  rules_end <- which(input == "") - 1
  rules_input <- input[1:rules_end]
  messages <- input[(rules_end + 2):length(input)]
  
  # Parse rules into a named list
  rules <- list()
  for (rule_line in rules_input) {
    parts <- strsplit(rule_line, ": ")[[1]]
    rules[[parts[1]]] <- parts[2]
  }
  
  # --- Part 1 ---
  regex_pattern <- paste0("^", build_regex(rules), "$")
  matches_part1 <- grepl(regex_pattern, messages)
  cat("Part 1:", sum(matches_part1), "\n")
  
  # --- Part 2 ---
  
  # Update rules 8 and 11.  Critically, we can't expand these infinitely.
  # Instead, we make multiple copies.  The number of copies is limited, but
  # is large enough for most cases.  If not enough copies, increase max_repeats.
  
  max_repeats <- 5 # Number of times to 'unroll' recursive rules
  rules[["8"]] <- paste(rep("42", 1), collapse = " ")
  for(i in 2:max_repeats){
    rules[["8"]] <- paste0(rules[["8"]], " | ", paste(rep("42", i), collapse = " "))
  }

  rules[["11"]] <- paste("42 31")
  for (i in 2:max_repeats) {
      rules[["11"]] <- paste0(rules[["11"]], " | ", paste(rep("42", i), collapse = " "), " ", paste(rep("31", i), collapse=" "))
  }
  
  
  regex_pattern2 <- paste0("^", build_regex(rules), "$")
  matches_part2 <- grepl(regex_pattern2, messages)
  cat("Part 2:", sum(matches_part2), "\n")
}

# Run the main function
main()
