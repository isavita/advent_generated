read_and_parse_input <- function(filename) {
  lines <- readLines(filename, warn = FALSE)
  distances <- list()
  
  for (line in lines) {
    parts <- unlist(strsplit(line, " "))
    if (length(parts) != 5) next
    
    from <- parts[1]
    to <- parts[3]
    distance <- as.integer(parts[5])
    
    distances[[from]][[to]] <- distance
    distances[[to]][[from]] <- distance
  }
  
  return(distances)
}

get_unique_locations <- function(distances) {
  unique(unlist(lapply(distances, names)))
}

find_longest_route <- function(locations, distances) {
  max_distance <- 0
  max_distance <- permute(locations, 1, max_distance, distances)
  return(max_distance)
}

permute <- function(arr, i, max_distance, distances) {
  if (i > length(arr)) return(max_distance)
  
  if (i == length(arr)) {
    dist <- calculate_route_distance(arr, distances)
    return(max(max_distance, dist))
  }
  
  for (j in i:length(arr)) {
    arr[c(i, j)] <- arr[c(j, i)]
    max_distance <- permute(arr, i + 1, max_distance, distances)
    arr[c(i, j)] <- arr[c(j, i)]
  }
  
  return(max_distance)
}

calculate_route_distance <- function(route, distances) {
  sum <- 0
  for (i in 1:(length(route) - 1)) {
    sum <- sum + distances[[route[i]]][[route[i + 1]]]
  }
  return(sum)
}

distances <- read_and_parse_input("input.txt")
locations <- get_unique_locations(distances)
max_distance <- find_longest_route(locations, distances)
print(max_distance)