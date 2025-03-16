
# Function to parse a single reaction line
parse_reaction <- function(line) {
  parts <- strsplit(line, " => ")[[1]]
  inputs <- strsplit(parts[1], ", ")[[1]]
  output <- strsplit(parts[2], " ")[[1]]
  
  input_list <- lapply(inputs, function(x) {
    y <- strsplit(x, " ")[[1]]
    list(name = y[2], quantity = as.integer(y[1]))
  })
  
  return(list(
    inputs = input_list,
    output = list(name = output[2], quantity = as.integer(output[1]))
  ))
}

# Function to calculate ORE required for 1 FUEL
calculate_ore <- function(reactions) {
  needs <- list(FUEL = 1)
  have <- list()
  ore_needed <- 0
  
  while (length(needs) > 0) {
    next_need_name <- names(needs)[1]
    next_need_quantity <- needs[[next_need_name]]
    needs[[next_need_name]] <- NULL
    
    if (next_need_name == "ORE") {
      ore_needed <- ore_needed + next_need_quantity
      next
    }
    
    if (!is.null(have[[next_need_name]]) && have[[next_need_name]] >= next_need_quantity) {
      have[[next_need_name]] <- have[[next_need_name]] - next_need_quantity
      next
    }
    
    needed_quantity <- next_need_quantity
    if (!is.null(have[[next_need_name]])) {
      needed_quantity <- needed_quantity - have[[next_need_name]]
      have[[next_need_name]] <- 0
    }

    
    reaction <- reactions[[next_need_name]]
    multiplier <- ceiling(needed_quantity / reaction$output$quantity)
    
    for (input in reaction$inputs) {
      if (is.null(needs[[input$name]])) {
        needs[[input$name]] <- 0
      }
      needs[[input$name]] <- needs[[input$name]] + (input$quantity * multiplier)
    }
    
    produced <- reaction$output$quantity * multiplier
    
    if(produced > needed_quantity)
    {
        if(is.null(have[[next_need_name]])){
            have[[next_need_name]] <- 0
        }
        have[[next_need_name]] <- have[[next_need_name]] + (produced - needed_quantity)
    }
  }
  
  return(ore_needed)
}


# Main function
main <- function() {
  # Read input from file
  lines <- readLines("input.txt")
  
  # Parse reactions
  reactions_list <- lapply(lines, parse_reaction)
  reactions <- list()
  for (reaction in reactions_list) {
    reactions[[reaction$output$name]] <- reaction
  }
  
  # Calculate ORE required
  ore_required <- calculate_ore(reactions)
  
  # Print the result
  cat(ore_required, "\n")
}

# Run the main function
main()
