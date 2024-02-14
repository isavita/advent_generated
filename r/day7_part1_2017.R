
data <- readLines("input.txt")

parse_input <- function(line) {
  parts <- strsplit(line, " -> ")[[1]]
  name_weight <- strsplit(parts[1], " \\(")[[1]]
  name <- name_weight[1]
  weight <- as.numeric(gsub("\\)", "", strsplit(name_weight[2], "\\(")[[1]]))
  
  above <- if(length(parts) == 2) strsplit(parts[2], ", ")[[1]] else NULL
  
  return(list(name = name, weight = weight, above = above))
}

programs <- lapply(data, parse_input)

names <- unlist(lapply(programs, function(x) x$name))
above <- unique(unlist(lapply(programs, function(x) x$above)))

bottom_program <- setdiff(names, above)
print(bottom_program)
