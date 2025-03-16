
# Function to check if a state is valid
is_valid_state <- function(state) {
    for (floor in 1:4) {
        items <- state[state[, 1] == floor, 2]
        generators <- grep("G", items)
        microchips <- grep("M", items)
        
        if (length(generators) > 0 && length(microchips) > 0) {
            for (m in microchips) {
                chip_type <- sub("M", "", items[m])
                if (!(paste0(chip_type, "G") %in% items)) {
                    return(FALSE)
                }
            }
        }
    }
    return(TRUE)
}

# Function to generate a unique key for a state
get_state_key <- function(state, elevator) {
    key <- paste(elevator, ":", sep = "")
    pairs <- c()
    
    for (i in 1:nrow(state)){
      if(grepl("G", state[i,2])){
        genType <- gsub("G", "", state[i,2])
        genFloor <- state[i,1]
        
        
        for(j in 1:nrow(state)){
          
          if(grepl("M", state[j,2])){
          
            microType <- gsub("M", "", state[j,2])
            microFloor <- state[j,1]
            
            if(genType == microType){
              pairs <- c(pairs, paste(genFloor, microFloor,sep=","))
            }
          }
        }
      }
    }

    key <- paste(key, paste(sort(pairs), collapse = ";"), sep="")
    
    return(key)
}

# Function to check if all items are on the 4th floor
is_final_state <- function(state) {
  return(all(state[, 1] == 4))
}

# Main function
main <- function() {
    # Read input from file
    input_lines <- readLines("input.txt")
    initial_state <- matrix(ncol = 2, nrow = 0)
    
    for (i in 1:length(input_lines)) {
        line <- input_lines[i]
        parts <- strsplit(line, " ")[[1]]
        
        for (j in 1:length(parts)) {
          
          if(grepl("generator", parts[j])){
            type <- gsub("-compatible", "", parts[j-1])
            
            initial_state <- rbind(initial_state, c(i, paste0(substr(type, 1, 2), "G")))
          }
          if(grepl("microchip", parts[j])){
            type <- gsub("-compatible", "", parts[j-1])
            initial_state <- rbind(initial_state, c(i, paste0(substr(type, 1, 2), "M")))
            
          }
        }
    }
    
    initial_state <- data.frame(floor=as.integer(initial_state[,1]), item=initial_state[,2], stringsAsFactors = FALSE)
    initial_elevator <- 1
    
    queue <- list(list(state = initial_state, elevator = initial_elevator, steps = 0))
    visited <- c()
    
    while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]
        
        current_state <- current$state
        current_elevator <- current$elevator
        current_steps <- current$steps
        
        state_key <- get_state_key(current_state, current_elevator)
        if (state_key %in% visited) {
            next
        }
        visited <- c(visited, state_key)
        
        if (is_final_state(current_state)) {
            cat("Minimum steps:", current_steps, "\n")
            return()
        }
        
        # Possible moves: up or down
        for (next_elevator in c(current_elevator - 1, current_elevator + 1)) {
            if (next_elevator >= 1 && next_elevator <= 4) {
                # Items to move
                current_floor_items <- current_state[current_state[, 1] == current_elevator, 2]
                
                # All combinations of 1 or 2 items
                for (i in 1:length(current_floor_items)) {
                  
                  moves <- list()  
                  moves[[1]] <- current_floor_items[i]
                  
                    for (j in 1:length(current_floor_items)) {
                        if (i != j) {
                          
                          moves[[length(moves) + 1]] <- sort(c(current_floor_items[i], current_floor_items[j]))

                        }
                    }

                    # Try each move
                    for (move in moves) {
                      
                        new_state <- current_state
                        
                        # Update the state after the move
                        for (item in move){
                          new_state[new_state[,2] == item, 1] <- next_elevator
                        }

                        
                        if (is_valid_state(new_state)) {
                          
                          queue[[length(queue) + 1]] <- list(state = new_state, elevator = next_elevator, steps = current_steps + 1)
                        }
                    }
                }
            }
        }
    }
}


main()
