
# Function to evaluate the circuit
evaluate_circuit <- function(instructions, overrides = list()) {
  signals <- list()
  
  # Apply overrides
  for (wire in names(overrides)) {
    signals[[wire]] <- overrides[[wire]]
  }

  while (!("a" %in% names(signals))) {
    for (instruction in instructions) {
      parts <- strsplit(instruction, " ")[[1]]
      
      if (length(parts) == 3 && parts[2] == "->") {
        # Assignment: value -> wire
        wire <- parts[3]
        if (!(wire %in% names(signals))) {
          value <- suppressWarnings(as.integer(parts[1]))
          if (!is.na(value)) {
            signals[[wire]] <- value
          } else if (parts[1] %in% names(signals)) {
            signals[[wire]] <- signals[[parts[1]]]
          }
        }
      } else if (length(parts) == 4 && parts[1] == "NOT") {
        # NOT operation: NOT wire -> wire
        input_wire <- parts[2]
        output_wire <- parts[4]
        if (!(output_wire %in% names(signals)) && input_wire %in% names(signals)) {
          signals[[output_wire]] <- bitwNot(signals[[input_wire]]) %% 65536
        }
      } else if (length(parts) == 5) {
        # Binary operations: wire/value OP wire/value -> wire
        left_operand <- parts[1]
        operator <- parts[2]
        right_operand <- parts[3]
        output_wire <- parts[5]
        
        if (!(output_wire %in% names(signals))) {
          left_val <- suppressWarnings(as.integer(left_operand))
          if (is.na(left_val)) {
            if (left_operand %in% names(signals)){
              left_val <- signals[[left_operand]]
            } else {
              next #skip the instruction if the signal value not ready
            }
          }
          
          right_val <- suppressWarnings(as.integer(right_operand))
          if (is.na(right_val)) {
            if(right_operand %in% names(signals)){
              right_val <- signals[[right_operand]]
            } else {
              next #skip the instruction if the signal value not ready
            }
          }
          
          if(!is.na(left_val) && !is.na(right_val)) {  
            if (operator == "AND") {
              signals[[output_wire]] <- bitwAnd(left_val, right_val)
            } else if (operator == "OR") {
              signals[[output_wire]] <- bitwOr(left_val, right_val)
            } else if (operator == "LSHIFT") {
              signals[[output_wire]] <- bitwShiftL(left_val, right_val)
            } else if (operator == "RSHIFT") {
              signals[[output_wire]] <- bitwShiftR(left_val, right_val)
            }
          }
        }
      }
    }
  }
  return(signals)
}


# Main function
main <- function() {
  # Read instructions from file
  instructions <- readLines("input.txt")

  # Part 1
  signals_part1 <- evaluate_circuit(instructions)
  cat("Part 1: Signal on wire 'a':", signals_part1[["a"]], "\n")

  # Part 2
  signals_part2 <- evaluate_circuit(instructions, overrides = list(b = signals_part1[["a"]]))
  cat("Part 2: Signal on wire 'a':", signals_part2[["a"]], "\n")
}

# Set 'main' as the entry point
if (interactive()) {
  # Running interactively (e.g., in RStudio)
  main()
} else {
  # Running non-interactively (e.g., from the command line)
  main()
}
