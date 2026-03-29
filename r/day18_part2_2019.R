
# Advent of Code - Day 18 - Many-Worlds Interpretation
# Focus: Part 2 - Minimal steps to collect all keys using four robots.

# Use scipen to prevent scientific notation when converting large state values to strings
options(scipen = 999)

# Standard binary heap implementation for Dijkstra's algorithm
heap_push <- function(heap, item) {
    heap$len <- heap$len + 1
    # Dynamically expand heap if needed
    if (heap$len > length(heap$data)) {
        heap$data <- c(heap$data, vector("list", length(heap$data)))
    }
    heap$data[[heap$len]] <- item
    
    idx <- heap$len
    while (idx > 1) {
        parent <- idx %/% 2
        if (heap$data[[idx]]$priority < heap$data[[parent]]$priority) {
            tmp <- heap$data[[idx]]
            heap$data[[idx]] <- heap$data[[parent]]
            heap$data[[parent]] <- tmp
            idx <- parent
        } else {
            break
        }
    }
    # Update heap in parent environment
    assign("heap", heap, envir = parent.frame())
}

heap_pop <- function(heap) {
    if (heap$len == 0) return(NULL)
    top <- heap$data[[1]]
    heap$data[[1]] <- heap$data[[heap$len]]
    heap$len <- heap$len - 1
    
    idx <- 1
    while (TRUE) {
        left <- 2 * idx
        right <- 2 * idx + 1
        smallest <- idx
        if (left <= heap$len && heap$data[[left]]$priority < heap$data[[smallest]]$priority) {
            smallest <- left
        }
        if (right <= heap$len && heap$data[[right]]$priority < heap$data[[smallest]]$priority) {
            smallest <- right
        }
        if (smallest != idx) {
            tmp <- heap$data[[idx]]
            heap$data[[idx]] <- heap$data[[smallest]]
            heap$data[[smallest]] <- tmp
            idx <- smallest
        } else {
            break
        }
    }
    assign("heap", heap, envir = parent.frame())
    return(top)
}

# Mapping characters to bitmasks
key_to_bit <- function(char) {
    bitwShiftL(1, utf8ToInt(char) - 97)
}
door_to_bit <- function(char) {
    bitwShiftL(1, utf8ToInt(char) - 65)
}

# BFS to pre-calculate distances and required doors/keys between POIs
bfs_poi <- function(start_r, start_c, map, poi_names, char_to_idx) {
    visited <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
    visited[start_r, start_c] <- TRUE
    queue <- list(list(r = start_r, c = start_c, dist = 0, mask = 0))
    head <- 1
    results <- list()
    
    while (head <= length(queue)) {
        curr <- queue[[head]]
        head <- head + 1
        
        for (dir in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
            nr <- curr$r + dir[1]
            nc <- curr$c + dir[2]
            
            if (nr < 1 || nr > nrow(map) || nc < 1 || nc > ncol(map)) next
            char <- map[nr, nc]
            if (char == "#" || visited[nr, nc]) next
            
            visited[nr, nc] <- TRUE
            new_mask <- curr$mask
            
            # Doors add to the required bitmask
            if (char %in% LETTERS) {
                new_mask <- bitwOr(new_mask, door_to_bit(char))
            }
            
            # Keys are recorded as targets, and also block paths until collected
            if (char %in% letters) {
                bit <- key_to_bit(char)
                results[[length(results) + 1]] <- list(idx = char_to_idx[char], 
                                                        dist = curr$dist + 1, 
                                                        mask = curr$mask, 
                                                        bit = bit)
                new_mask <- bitwOr(new_mask, bit)
            }
            
            queue[[length(queue) + 1]] <- list(r = nr, c = nc, dist = curr$dist + 1, mask = new_mask)
        }
    }
    return(results)
}

# Main function
solve <- function() {
    input_file <- "input.txt"
    if (!file.exists(input_file)) return()
    lines <- readLines(input_file)
    map <- do.call(rbind, strsplit(lines, ""))
    
    # Locate original entrance and transform map for Part 2 if necessary
    center <- which(map == "@", arr.ind = TRUE)
    if (nrow(center) == 1) {
        r <- center[1, 1]; c <- center[1, 2]
        map[r, c] <- "#"
        map[r-1, c] <- "#"; map[r+1, c] <- "#"
        map[r, c-1] <- "#"; map[r, c+1] <- "#"
        map[r-1, c-1] <- "@"; map[r-1, c+1] <- "@"
        map[r+1, c-1] <- "@"; map[r+1, c+1] <- "@"
    }
    
    # Identify all Points of Interest (POIs)
    starts <- which(map == "@", arr.ind = TRUE)
    unique_keys <- sort(unique(map[map %in% letters]))
    
    poi_names <- character(0)
    for (i in 1:nrow(starts)) poi_names <- c(poi_names, paste0("@", i))
    poi_names <- c(poi_names, unique_keys)
    
    char_to_idx <- seq_along(poi_names)
    names(char_to_idx) <- poi_names
    
    poi_coords <- list()
    for (i in 1:nrow(starts)) poi_coords[[paste0("@", i)]] <- starts[i, ]
    for (k in unique_keys) poi_coords[[k]] <- which(map == k, arr.ind = TRUE)[1, ]
    
    all_keys_mask <- 0
    for (k in unique_keys) all_keys_mask <- bitwOr(all_keys_mask, key_to_bit(k))
    
    # Pre-calculate adjacency (reachable keys) for each POI
    adj_list <- lapply(poi_names, function(name) {
        coords <- poi_coords[[name]]
        bfs_poi(coords[1], coords[2], map, poi_names, char_to_idx)
    })
    
    # Dijkstra's Search
    # State: mask * 2^20 + p1 * 2^15 + p2 * 2^10 + p3 * 2^5 + p4
    get_state_val <- function(pos, mask) {
        p <- rep(0, 4)
        p[seq_along(pos)] <- pos - 1 # 0-indexed for bit-packing
        mask * 1048576 + p[1] * 32768 + p[2] * 1024 + p[3] * 32 + p[4]
    }
    
    initial_pos <- 1:nrow(starts)
    heap <- list(data = vector("list", 200000), len = 0)
    heap_push(heap, list(priority = 0, pos = initial_pos, mask = 0))
    
    visited <- new.env(hash = TRUE, parent = emptyenv())
    
    while (heap$len > 0) {
        curr <- heap_pop(heap)
        
        if (curr$mask == all_keys_mask) {
            cat(curr$priority, "\n")
            return()
        }
        
        state_key <- as.character(get_state_val(curr$pos, curr$mask))
        if (exists(state_key, visited)) next
        visited[[state_key]] <- TRUE
        
        # Explore moving any of the robots
        for (ri in 1:length(curr$pos)) {
            curr_poi_idx <- curr$pos[ri]
            for (move in adj_list[[curr_poi_idx]]) {
                # Check if key is uncollected and path is clear (doors unlocked)
                if (bitwAnd(curr$mask, move$bit) == 0) {
                    if (bitwAnd(move$mask, curr$mask) == move$mask) {
                        new_pos <- curr$pos
                        new_pos[ri] <- move$idx
                        new_mask <- bitwOr(curr$mask, move$bit)
                        
                        # Only push to heap if state not visited
                        new_state_key <- as.character(get_state_val(new_pos, new_mask))
                        if (!exists(new_state_key, visited)) {
                            heap_push(heap, list(priority = curr$priority + move$dist, 
                                                 pos = new_pos, 
                                                 mask = new_mask))
                        }
                    }
                }
            }
        }
    }
}

# Execute
solve()
