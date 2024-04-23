firstRevisitedDistance <- function(instructions) {
  pos <- c(0, 0)
  visited <- list()
  visited[[paste(pos[1], pos[2], sep = ",")]] <- TRUE  # Fixed the assignment
  directions <- list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))
  dirIndex <- 1

  for (instruction in instructions) {
    turn <- substr(instruction, 1, 1)
    blocks <- as.integer(substring(instruction, 2))

    if (turn == "R") {
      dirIndex <- (dirIndex %% 4) + 1
    } else {
      dirIndex <- ((dirIndex - 2) %% 4) + 1
    }

    for (i in 1:blocks) {
      pos[1] <- pos[1] + directions[[dirIndex]][1]
      pos[2] <- pos[2] + directions[[dirIndex]][2]

      key <- paste(pos[1], pos[2], sep = ",")
      if (key %in% names(visited)) {
        return(abs(pos[1]) + abs(pos[2]))
      }
      visited[[key]] <- TRUE
    }
  }

  return(-1) # No location visited twice
}

instructions <- strsplit(readLines("input.txt"), ", ")[[1]]
print(firstRevisitedDistance(instructions))