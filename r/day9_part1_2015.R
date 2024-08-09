read_and_parse_input <- function(filename) {
  lines <- readLines(filename)
  distances <- list()
  
  for (line in lines) {
    parts <- strsplit(line, " ")[[1]]
    if (length(parts) != 5) next
    from <- parts[1]
    to <- parts[3]
    distance <- as.integer(parts[5])
    
    if (!from %in% names(distances)) distances[[from]] <- list()
    if (!to %in% names(distances)) distances[[to]] <- list()
    
    distances[[from]][[to]] <- distance
    distances[[to]][[from]] <- distance
  }
  
  distances
}

get_unique_locations <- function(distances) {
  unique(unlist(lapply(names(distances), function(x) c(x, names(distances[[x]])))))
}

find_shortest_route <- function(locations, distances) {
  min_distance <- Inf
  permute(locations, 1, min_distance, distances)
}

permute <- function(arr, i, min_distance, distances) {
  if (i > length(arr)) return(min_distance)
  if (i == length(arr)) {
    dist <- calculate_route_distance(arr, distances)
    return(min(min_distance, dist))
  }
  
  for (j in i:length(arr)) {
    arr[c(i, j)] <- arr[c(j, i)]
    min_distance <- permute(arr, i + 1, min_distance, distances)
    arr[c(i, j)] <- arr[c(j, i)]
  }
  
  min_distance
}

calculate_route_distance <- function(route, distances) {
  sum(sapply(1:(length(route) - 1), function(i) distances[[route[i]]][[route[i + 1]]]))
}

distances <- read_and_parse_input("input.txt")
locations <- get_unique_locations(distances)
min_distance <- find_shortest_route(locations, distances)
print(min_distance)