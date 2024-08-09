manhattan_distance <- function(p1, p2) {
  sum(abs(p1 - p2))
}

find_constellations <- function(points) {
  n <- nrow(points)
  constellations <- rep(NA, n)
  constellation_id <- 0
  
  for (i in 1:n) {
    if (is.na(constellations[i])) {
      constellation_id <- constellation_id + 1
      queue <- list(i)
      
      while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]
        
        if (is.na(constellations[current])) {
          constellations[current] <- constellation_id
          
          for (j in 1:n) {
            if (is.na(constellations[j]) && manhattan_distance(points[current, ], points[j, ]) <= 3) {
              queue <- append(queue, list(j))
            }
          }
        }
      }
    }
  }
  
  length(unique(constellations))
}

main <- function() {
  points <- read.table("input.txt", sep = ",", header = FALSE)
  result <- find_constellations(as.matrix(points))
  cat(result, "\n")
}

main()