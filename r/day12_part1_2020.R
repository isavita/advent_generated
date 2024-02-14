
input <- readLines("input.txt")
ship <- c(0, 0)
facing <- 0

for (line in input) {
  action <- substr(line, 1, 1)
  value <- as.integer(substr(line, 2, nchar(line)))
  
  switch(action,
         "N" = ship[2] <- ship[2] + value,
         "S" = ship[2] <- ship[2] - value,
         "E" = ship[1] <- ship[1] + value,
         "W" = ship[1] <- ship[1] - value,
         "L" = facing <- (facing - value + 360) %% 360,
         "R" = facing <- (facing + value) %% 360,
         "F" = {
           if (facing == 0) {
             ship[1] <- ship[1] + value
           } else if (facing == 90) {
             ship[2] <- ship[2] - value
           } else if (facing == 180) {
             ship[1] <- ship[1] - value
           } else if (facing == 270) {
             ship[2] <- ship[2] + value
           }
         }
  )
}

manhattanDistance <- abs(ship[1]) + abs(ship[2])
cat(manhattanDistance, "\n")
