
add_outer <- function(label, side, x, y) {
  key <- if (label %in% c("up", "down")) c(y, x) else c(x, y)
  side[[label]] <- rbind(side[[label]], key)
  return(side)
}

count_outer <- function(side) {
  outer <- 0
  for (label in names(side)) {
    keys <- side[[label]]
    if (is.null(keys) || nrow(keys) == 0) next
    sorted_keys <- keys[order(keys[, 1], keys[, 2]), , drop = FALSE]
    temp <- matrix(nrow = 0, ncol = 2)
    for (i in 1:nrow(sorted_keys)) {
      key <- sorted_keys[i, ]
      if (!check(temp, key)) {
        outer <- outer + 1
      }
      temp <- rbind(temp, key)
    }
  }
  return(outer)
}

check <- function(ary, key) {
    if (nrow(ary) == 0) return(FALSE)
    
    i <- key[1]
    j <- key[2]
  
    for (k in 1:nrow(ary)){
        di <- ary[k,1]
        dj <- ary[k,2]
        if((di == i & dj == j-1) | (di == i & dj == j+1) | (di == i-1 & dj == j) | (di == i+1 & dj == j)){
            return(TRUE)
        }
    }
    return(FALSE)
}

get_label <- function(dx, dy) {
  if (dx == -1) return("left")
  if (dx == 1) return("right")
  if (dy == -1) return("up")
  return("down")
}

solve <- function() {
  lines <- readLines("input.txt")
  graph <- do.call(rbind, strsplit(lines, ""))
  H <- nrow(graph)
  W <- ncol(graph)
  
  moves <- matrix(c(-1, 0, 0, -1, 1, 0, 0, 1), ncol = 2, byrow = TRUE)
  total_sum <- 0
  
  for (y in 1:H) {
    for (x in 1:W) {
      if (graph[y, x] == '.') next
      
      area <- 0
      target <- graph[y, x]
      visited <- matrix(nrow = 0, ncol = 2)
      side <- list(left = matrix(nrow = 0, ncol = 2), up = matrix(nrow = 0, ncol = 2),
                   right = matrix(nrow = 0, ncol = 2), down = matrix(nrow = 0, ncol = 2))
      
      q <- list(list(x = x, y = y, label = ""))
      while (length(q) > 0) {
        curr <- q[[1]]
        q <- q[-1]
        cx <- curr$x
        cy <- curr$y
        label <- curr$label
        
        if (graph[cy, cx] != target) {
          
          if(label != "" & !any(duplicated(rbind(visited,c(cx,cy)))) ){
                side <- add_outer(label, side, cx, cy)
          }

          next
        }

        if(any(duplicated(rbind(visited,c(cx,cy))))){
            next
        }
        
        visited <- rbind(visited,c(cx,cy))

        area <- area + 1
        graph[cy, cx] <- '.'
        
        for (i in 1:nrow(moves)) {
          dx <- moves[i, 1]
          dy <- moves[i, 2]
          nx <- cx + dx
          ny <- cy + dy
          
          if (nx >= 1 && nx <= W && ny >= 1 && ny <= H) {
            q <- c(q, list(list(x = nx, y = ny, label = get_label(dx, dy))))
          } else {

            side<- add_outer(get_label(dx,dy), side, nx, ny)
          }
        }
      }
      
      outer <- count_outer(side)
      total_sum <- total_sum + area * outer
    }
  }
  
  cat(total_sum, "\n")
}

solve()
