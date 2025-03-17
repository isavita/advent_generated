
read_maze <- function(file_path) {
    lines <- readLines(file_path)
    maze <- do.call(rbind, lapply(strsplit(lines, ""), function(x) x))
    return(maze)
}

find_portals <- function(maze) {
    portals <- list()
    rows <- nrow(maze)
    cols <- ncol(maze)

    # Helper function to check and add portal
    add_portal <- function(r, c, name) {
        if (!is.null(portals[[name]])) {
            portals[[name]] <<- rbind(portals[[name]], c(r, c))
        } else {
            portals[[name]] <<- matrix(c(r, c), nrow = 1, byrow = TRUE)
        }
    }
    
    #Check Horizontally
    for (r in 1:rows) {
      for(c in 1:(cols-2)){
        if(all(maze[r,c:(c+1)] %in% LETTERS)  & maze[r,c+2] == "."){
          name <- paste0(maze[r,c],maze[r,c+1])
          add_portal(r,c+2,name)
        } else if (maze[r,c] == "." & all(maze[r,c+1:2] %in% LETTERS)  ){
           name <- paste0(maze[r,c+1],maze[r,c+2])
           add_portal(r,c,name)
        }
      }
    }
    
    #Check Vertically
     for (c in 1:cols) {
      for(r in 1:(rows-2)){
        if(all(maze[r:(r+1),c] %in% LETTERS) & maze[r+2,c] == "."){
          name <- paste0(maze[r,c],maze[r+1,c])
          add_portal(r+2,c,name)
        } else if (maze[r,c] == "." & all(maze[r+1:2,c] %in% LETTERS)  ){
           name <- paste0(maze[r+1,c],maze[r+2,c])
           add_portal(r,c,name)
        }
      }
     }

    return(portals)
}

build_graph <- function(maze, portals) {
    graph <- list()
    rows <- nrow(maze)
    cols <- ncol(maze)
  
    add_edge <- function(from, to, weight = 1) {
      if (is.null(graph[[paste(from, collapse = ",")]])) {
          graph[[paste(from, collapse = ",")]] <<- list()
      }
      graph[[paste(from, collapse = ",")]] <<- c(graph[[paste(from, collapse = ",")]], list(list(node = to, weight = weight)))
    }

    # Add edges for walkable tiles
    for (r in 1:rows) {
        for (c in 1:cols) {
            if (maze[r, c] == ".") {
                if (r > 1 && maze[r - 1, c] == ".")       add_edge(c(r, c), c(r - 1, c))
                if (r < rows && maze[r + 1, c] == ".")    add_edge(c(r, c), c(r + 1, c))
                if (c > 1 && maze[r, c - 1] == ".")       add_edge(c(r, c), c(r, c - 1))
                if (c < cols && maze[r, c + 1] == ".")    add_edge(c(r, c), c(r, c + 1))
            }
        }
    }

    # Add edges for portals
    for (name in names(portals)) {
        if(nrow(portals[[name]]) > 1){
          coords <- portals[[name]]
          add_edge(coords[1, ], coords[2, ], 1)
          add_edge(coords[2, ], coords[1, ], 1)
        }
    }

    return(graph)
}

bfs <- function(graph, start, end) {
    queue <- list(list(node = start, dist = 0))
    visited <- matrix(FALSE, nrow = max(sapply(graph, function(x) max(sapply(x, function(y) y$node[1])))),
                      ncol =  max(sapply(graph, function(x) max(sapply(x, function(y) y$node[2])))))

    
    visited[start[1], start[2]] <- TRUE

    while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]

        if (all(current$node == end)) {
            return(current$dist)
        }
        
        node_key <- paste(current$node, collapse = ",")

        if (!is.null(graph[[node_key]])) {
            for (neighbor in graph[[node_key]]) {
                if (!visited[neighbor$node[1], neighbor$node[2]]) {
                    visited[neighbor$node[1], neighbor$node[2]] <- TRUE
                    queue <- c(queue, list(list(node = neighbor$node, dist = current$dist + neighbor$weight)))
                }
            }
        }
    }

    return(-1)  # No path found
}


main <- function() {
    maze <- read_maze("input.txt")
    portals <- find_portals(maze)
    graph <- build_graph(maze, portals)
    start_node <- portals[["AA"]][1,]
    end_node <- portals[["ZZ"]][1,]
    
    distance <- bfs(graph, start_node, end_node)
    cat(distance, "\n")
}


main()
