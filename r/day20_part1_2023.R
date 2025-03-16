
# Function to process a single pulse
process_pulse <- function(modules, state, pulse_queue) {
    low_pulses <- 0
    high_pulses <- 0
    
    while (length(pulse_queue) > 0) {
        pulse <- pulse_queue[[1]]
        pulse_queue <- pulse_queue[-1]
        
        from <- pulse$from
        to <- pulse$to
        type <- pulse$type
        
        if (type == 0) {
            low_pulses <- low_pulses + 1
        } else {
            high_pulses <- high_pulses + 1
        }
        
        if (!(to %in% names(modules))) {
          next
        }
        
        module_type <- modules[[to]]$type
        destinations <- modules[[to]]$destinations
        
        if (module_type == "broadcaster") {
            for (dest in destinations) {
                pulse_queue[[length(pulse_queue) + 1]] <- list(from = to, to = dest, type = type)
            }
        } else if (module_type == "%") {
            if (type == 0) {
                state[[to]] <- 1 - state[[to]]
                send_type <- state[[to]]
                for (dest in destinations) {
                    pulse_queue[[length(pulse_queue) + 1]] <- list(from = to, to = dest, type = send_type)
                }
            }
        } else if (module_type == "&") {
            state[[to]][[from]] <- type
            if (all(unlist(state[[to]]) == 1)) {
                send_type <- 0
            } else {
                send_type <- 1
            }
            for (dest in destinations) {
                pulse_queue[[length(pulse_queue) + 1]] <- list(from = to, to = dest, type = send_type)
            }
        }
    }
    
    return(list(low_pulses = low_pulses, high_pulses = high_pulses, state = state, pulse_queue = pulse_queue))
}

# Main function
main <- function() {
    # Read input from file
    lines <- readLines("input.txt")
    
    # Parse module configuration
    modules <- list()
    state <- list()
    
    for (line in lines) {
        parts <- strsplit(line, " -> ")[[1]]
        name <- parts[1]
        destinations <- strsplit(parts[2], ", ")[[1]]
        
        if (name == "broadcaster") {
            modules[[name]] <- list(type = "broadcaster", destinations = destinations)
        } else if (substr(name, 1, 1) == "%") {
            module_name <- substr(name, 2, nchar(name))
            modules[[module_name]] <- list(type = "%", destinations = destinations)
            state[[module_name]] <- 0  # Initially off
        } else if (substr(name, 1, 1) == "&") {
            module_name <- substr(name, 2, nchar(name))
            modules[[module_name]] <- list(type = "&", destinations = destinations)
            state[[module_name]] <- list()
        }
    }

    # Initialize conjunction module states
      for (module_name in names(modules)) {
        for (dest in modules[[module_name]]$destinations) {
          if (dest %in% names(modules) && modules[[dest]]$type == "&") {
            state[[dest]][[module_name]] <- 0  # Initially remember low pulse
          }
        }
      }
    
    total_low_pulses <- 0
    total_high_pulses <- 0
    
    # Simulate button presses
    for (i in 1:1000) {
        pulse_queue <- list(list(from = "button", to = "broadcaster", type = 0)) # low pulse
        result <- process_pulse(modules, state, pulse_queue)
        
        total_low_pulses <- total_low_pulses + result$low_pulses
        total_high_pulses <- total_high_pulses + result$high_pulses
        state <- result$state
    }
    
    # Calculate and print the product of low and high pulses
    cat(total_low_pulses * total_high_pulses, "\n")
}

# Set working directory to the directory of this script and call the main function

main()

