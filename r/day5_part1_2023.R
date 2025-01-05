
parse_input <- function(file_path) {
  lines <- readLines(file_path)
  seeds <- as.numeric(strsplit(gsub("seeds: ", "", lines[grep("seeds:", lines)]), " ")[[1]])
  maps <- list()
  current_map <- list()
  for (line in lines) {
    if (grepl("map:", line)) {
      if (length(current_map) > 0) {
        maps <- append(maps, list(do.call(rbind, current_map)))
        current_map <- list()
      }
    } else if (grepl("^[0-9]", line)) {
      nums <- as.numeric(strsplit(line, " ")[[1]])
      current_map <- append(current_map, list(nums))
    }
  }
  maps <- append(maps, list(do.call(rbind, current_map)))
  list(seeds = seeds, maps = maps)
}

convert_number <- function(number, ranges) {
  for (i in 1:nrow(ranges)) {
    if (number >= ranges[i, 2] && number < ranges[i, 2] + ranges[i, 3]) {
      return(ranges[i, 1] + (number - ranges[i, 2]))
    }
  }
  number
}

solve <- function(file_path) {
  data <- parse_input(file_path)
  seeds <- data$seeds
  maps <- data$maps
  locations <- numeric(length(seeds))
  for (i in seq_along(seeds)) {
    location <- seeds[i]
    for (map in maps) {
      location <- convert_number(location, map)
    }
    locations[i] <- location
  }
  min(locations)
}

result <- solve("input.txt")
cat(result, "\n")
