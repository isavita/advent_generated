
input <- readLines("input.txt")
directions <- unlist(strsplit(input, ","))

abs <- function(x) {
  if (x < 0) {
    return(-x)
  }
  return(x)
}

max <- function(a, b) {
  if (a > b) {
    return(a)
  }
  return(b)
}

distance <- function(x, y, z) {
  return((abs(x) + abs(y) + abs(z)) / 2)
}

x <- 0
y <- 0
z <- 0
maxDistance <- 0

for (dir in directions) {
  switch(dir,
         "n" = {
           y <- y + 1
           z <- z - 1
         },
         "ne" = {
           x <- x + 1
           z <- z - 1
         },
         "se" = {
           x <- x + 1
           y <- y - 1
         },
         "s" = {
           y <- y - 1
           z <- z + 1
         },
         "sw" = {
           x <- x - 1
           z <- z + 1
         },
         "nw" = {
           x <- x - 1
           y <- y + 1
         }
  )

  curDistance <- distance(x, y, z)
  maxDistance <- max(maxDistance, curDistance)
}

cat(maxDistance)
