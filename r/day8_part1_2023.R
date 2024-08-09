navigate_wasteland <- function(file_path) {
  lines <- readLines(file_path)
  instructions <- strsplit(lines[1], "")[[1]]
  
  nodes <- list()
  for (line in lines[-1]) {
    parts <- unlist(strsplit(line, " = "))
    node <- parts[1]
    connections <- gsub("[()]", "", parts[2])
    nodes[[node]] <- unlist(strsplit(connections, ", "))
  }
  
  current_node <- "AAA"
  steps <- 0
  instruction_length <- length(instructions)
  instruction_index <- 1
  
  while (current_node != "ZZZ") {
    direction <- instructions[instruction_index]
    current_node <- nodes[[current_node]][ifelse(direction == "R", 2, 1)]
    steps <- steps + 1
    instruction_index <- (instruction_index %% instruction_length) + 1
  }
  
  return(steps)
}

cat(navigate_wasteland("input.txt"), "\n")