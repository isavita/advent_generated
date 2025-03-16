
# Function to parse dependencies from input lines
parse_dependencies <- function(lines) {
  dependencies <- list()
  all_steps <- character()

  for (line in lines) {
    parts <- strsplit(line, " ")[[1]]
    before <- parts[2]
    after <- parts[8]
    
    all_steps <- unique(c(all_steps, before, after))

    if (is.null(dependencies[[after]])) {
      dependencies[[after]] <- c(before)
    } else {
      dependencies[[after]] <- c(dependencies[[after]], before)
    }
  }
    
  all_steps <- sort(all_steps)  # Ensure steps are sorted alphabetically
  return(list(dependencies = dependencies, all_steps = all_steps))
}


# Function to determine the order of steps (Part 1)
solve_part1 <- function(dependencies, all_steps) {
  
  order <- character()
  available <- all_steps
  
  while (length(available) > 0) {
      ready <- character()
      for(step in available){
          if(is.null(dependencies[[step]]) || all(dependencies[[step]] %in% order)){
              ready <- c(ready, step)
          }
      }
      
    ready <- sort(ready)
    
    if(length(ready) > 0) {
    next_step <- ready[1]
    order <- c(order, next_step)
    available <- setdiff(available, next_step)
    } else {
        break #should not reach here with valid input.  Added for safety.
    }
  }

  return(paste(order, collapse = ""))
}

# Function to calculate the time taken with multiple workers (Part 2)
solve_part2 <- function(dependencies, all_steps, num_workers, base_time) {
  time <- 0
  workers <- rep(0, num_workers)  # Time when each worker becomes free
  worker_tasks <- rep("", num_workers)  # Task each worker is currently doing
  completed <- character()
  available <- all_steps
  
  
   while (length(completed) < length(all_steps)) {
        # Find ready tasks
        ready <- character()
        for(step in available){
            if(is.null(dependencies[[step]]) || all(dependencies[[step]] %in% completed)){
                ready <- c(ready, step)
            }
        }
        ready <- sort(ready)
        
        # Assign tasks to available workers
        for (i in 1:num_workers) {
          if (workers[i] <= time && length(ready) > 0) {
            next_task <- ready[1]
            ready <- ready[-1]  # remove the selected
            available <- setdiff(available, next_task)
            
            workers[i] <- time + base_time + (match(next_task, LETTERS))
            worker_tasks[i] <- next_task
          }
        }

        # Find the next time a worker will finish.  Advance time.
        next_finish_time <- min(workers[workers > time])
          
        if(is.infinite(next_finish_time)){
            time <- time + 1  #idle time... shouldn't happen on valid input.  Added for robustness.
            next
        }
          
        time <- next_finish_time

        # Mark tasks as completed
        for (i in 1:num_workers) {
          if (workers[i] == time && worker_tasks[i] != "") {
            completed <- c(completed, worker_tasks[i])
            worker_tasks[i] <- "" #reset task for next loop.
          }
        }
  }

  return(time)
}


# Main function
main <- function() {
  # Read input from file
  input_file <- "input.txt"
  lines <- readLines(input_file)

  parsed <- parse_dependencies(lines)
  dependencies <- parsed$dependencies
  all_steps <- parsed$all_steps

  # Part 1
  part1_result <- solve_part1(dependencies, all_steps)
  cat("Part 1:", part1_result, "\n")

  # Part 2
  num_workers <- 5
  base_time <- 60
  part2_result <- solve_part2(dependencies, all_steps, num_workers, base_time)
  cat("Part 2:", part2_result, "\n")
}

# Run the main function
main()
