
# Function to get the value of a register or a number
get_value <- function(registers, x) {
  if (is.na(as.numeric(x))) {
    return(registers[[x]])
  } else {
    return(as.numeric(x))
  }
}

# Function to execute the Duet assembly code (Part 1)
solve_part1 <- function(instructions) {
  registers <- new.env(parent = emptyenv())
  for (reg in letters) {
    registers[[reg]] <- 0
  }
  
  last_sound <- 0
  pc <- 1 # Program counter
  
  while (pc >= 1 && pc <= length(instructions)) {
    instr <- instructions[[pc]]
    op <- instr[1]
    x <- instr[2]
    y <- if (length(instr) > 2) instr[3] else NA
    
    if (op == "snd") {
      last_sound <- get_value(registers, x)
    } else if (op == "set") {
      registers[[x]] <- get_value(registers, y)
    } else if (op == "add") {
      registers[[x]] <- registers[[x]] + get_value(registers, y)
    } else if (op == "mul") {
      registers[[x]] <- registers[[x]] * get_value(registers, y)
    } else if (op == "mod") {
      registers[[x]] <- registers[[x]] %% get_value(registers, y)
    } else if (op == "rcv") {
      if (get_value(registers, x) != 0) {
        return(last_sound)
      }
    } else if (op == "jgz") {
      if (get_value(registers, x) > 0) {
        pc <- pc + get_value(registers, y)
        next
      }
    }
    pc <- pc + 1
  }
  return(NULL) # Should not reach here in normal execution for Part 1
}

# Function to execute the Duet assembly code (Part 2)
solve_part2 <- function(instructions) {
    registers <- list(new.env(parent = emptyenv()), new.env(parent = emptyenv()))
    for (i in 1:2) {
        for (reg in letters) {
            registers[[i]][[reg]] <- 0
        }
        registers[[i]][["p"]] <- i - 1  # Program ID
    }

    pcs <- c(1, 1)  # Program counters
    queues <- list(list(), list())
    send_counts <- c(0, 0)
    current_program <- 1
    waiting <- c(FALSE, FALSE)
    
    while (TRUE) {
        
        if(all(waiting) || any(pcs < 1 | pcs > length(instructions))){
           break;
        }

        if(pcs[current_program] < 1 || pcs[current_program] > length(instructions)){
          current_program <- 3 - current_program
          next;
        }


        instr <- instructions[[pcs[current_program]]]
        op <- instr[1]
        x <- instr[2]
        y <- if (length(instr) > 2) instr[3] else NA

        if (op == "snd") {
            send_counts[current_program] <- send_counts[current_program] + 1
            other_program <- 3 - current_program
            queues[[other_program]] <- append(queues[[other_program]], get_value(registers[[current_program]], x))
            waiting[other_program] <- FALSE #The other program is no longer waiting
        } else if (op == "set") {
            registers[[current_program]][[x]] <- get_value(registers[[current_program]], y)
        } else if (op == "add") {
            registers[[current_program]][[x]] <- registers[[current_program]][[x]] + get_value(registers[[current_program]], y)
        } else if (op == "mul") {
            registers[[current_program]][[x]] <- registers[[current_program]][[x]] * get_value(registers[[current_program]], y)
        } else if (op == "mod") {
            registers[[current_program]][[x]] <- registers[[current_program]][[x]] %% get_value(registers[[current_program]], y)
        } else if (op == "rcv") {
            if (length(queues[[current_program]]) > 0) {
                registers[[current_program]][[x]] <- queues[[current_program]][[1]]
                queues[[current_program]] <- queues[[current_program]][-1]
                waiting[current_program] = FALSE
            } else {
                waiting[current_program] = TRUE
                current_program <- 3 - current_program # Switch to other program
                next
            }
        } else if (op == "jgz") {
            if (get_value(registers[[current_program]], x) > 0) {
                pcs[current_program] <- pcs[current_program] + get_value(registers[[current_program]], y)
                next
            }
        }

        pcs[current_program] <- pcs[current_program] + 1
        
    }

    return(send_counts[2])  # Return the number of times program 1 sent a value
}


# Main function to read input and call the solvers
main <- function() {
  file_content <- readLines("input.txt")
  instructions <- strsplit(file_content, " ")
  
  part1_result <- solve_part1(instructions)
  cat("Part 1:", part1_result, "\n")
  
  part2_result <- solve_part2(instructions)
  cat("Part 2:", part2_result, "\n")
}

# Run the main function
main()
