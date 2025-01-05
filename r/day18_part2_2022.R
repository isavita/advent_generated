
input <- readLines("input.txt")
input <- input[input != ""]
cubes <- do.call(rbind, lapply(input, function(x) as.integer(strsplit(x, ",")[[1]])))
min_coords <- apply(cubes, 2, min) - 1
max_coords <- apply(cubes, 2, max) + 1
neighbors <- matrix(c(-1,0,0, 1,0,0, 0,-1,0, 0,1,0, 0,0,-1, 0,0,1), ncol=3, byrow=TRUE)
q <- list(min_coords)
seen <- list(paste(min_coords, collapse=","))
faces <- 0

while(length(q) > 0) {
  curr <- q[[1]]
  q <- q[-1]
  for(i in 1:nrow(neighbors)) {
    next_coord <- curr + neighbors[i,]
    if(any(next_coord < min_coords) || any(next_coord > max_coords)) {
      next
    }
    if(any(apply(cubes, 1, function(x) all(x == next_coord)))) {
      faces <- faces + 1
    } else {
      next_coord_str <- paste(next_coord, collapse=",")
      if(!next_coord_str %in% seen) {
        seen <- c(seen, next_coord_str)
        q <- c(q, list(next_coord))
      }
    }
  }
}
cat(faces, "\n")
