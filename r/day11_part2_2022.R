
parse_input <- function(file_path) {
  lines <- readLines(file_path)
  monkeys <- list()
  i <- 1
  while (i <= length(lines)) {
    if (startsWith(lines[i], "Monkey")) {
      monkey_id <- as.integer(gsub("[^0-9]", "", lines[i]))
      i <- i + 1
      
      items <- as.numeric(strsplit(gsub("Starting items: ", "", lines[i]), ", ")[[1]])
      i <- i + 1
      
      op_line <- gsub("  Operation: new = ", "", lines[i])
      operation <- strsplit(op_line, " ")[[1]]
      i <- i + 1
      
      test_divisible <- as.numeric(gsub("[^0-9]", "", lines[i]))
      i <- i + 1
      
      true_target <- as.integer(gsub("[^0-9]", "", lines[i]))
      i <- i + 1
      
      false_target <- as.integer(gsub("[^0-9]", "", lines[i]))
      i <- i + 1
      
      monkeys[[as.character(monkey_id)]] <- list(
        items = items,
        operation = operation,
        test_divisible = test_divisible,
        true_target = true_target,
        false_target = false_target,
        inspections = 0
      )
    } else {
      i <- i + 1
    }
  }
  return(monkeys)
}

perform_operation <- function(old, operation) {
    operand1 <- ifelse(operation[1] == "old", old, as.numeric(operation[1]))
    operator <- operation[2]
    operand2 <- ifelse(operation[3] == "old", old, as.numeric(operation[3]))

    if (operator == "+") {
        return(operand1 + operand2)
    } else if (operator == "*") {
        return(operand1 * operand2)
    }
    return(old) # Should not happen
}

solve <- function(file_path, rounds, divide_by_three = TRUE) {
  monkeys <- parse_input(file_path)
  
  # For Part 2: Calculate the LCM (product) of all divisors
  lcm <- 1
  for (monkey in monkeys) {
    lcm <- lcm * monkey$test_divisible
  }

  for (round in 1:rounds) {
    for (monkey_id in names(monkeys)) {
      monkey <- monkeys[[monkey_id]]
      for (item in monkey$items) {
        monkeys[[monkey_id]]$inspections <- monkeys[[monkey_id]]$inspections + 1
        new_worry <- perform_operation(item, monkey$operation)
        
        if (divide_by_three) {
          new_worry <- floor(new_worry / 3)
        } else {
            new_worry <- new_worry %% lcm # Keep worry levels manageable
        }

        if (new_worry %% monkey$test_divisible == 0) {
          target <- as.character(monkey$true_target)
        } else {
          target <- as.character(monkey$false_target)
        }
        monkeys[[target]]$items <- c(monkeys[[target]]$items, new_worry)
      }
      monkeys[[monkey_id]]$items <- numeric(0) # Clear items after processing
    }
  }

  inspections <- sapply(monkeys, function(x) x$inspections)
  sorted_inspections <- sort(inspections, decreasing = TRUE)
  return(sorted_inspections[1] * sorted_inspections[2])
}

main <- function() {
  cat("Part 1:", solve("input.txt", 20, TRUE), "\n")       # Part 1
  cat("Part 2:", solve("input.txt", 10000, FALSE), "\n")  # Part 2
}

main()
