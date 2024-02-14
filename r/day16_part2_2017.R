
input <- readLines("input.txt")
moves <- strsplit(input, ",")[[1]]

programs <- strsplit("abcdefghijklmnop", "")[[1]]
initial <- programs
cycleLen <- 0

for (i in 1:1000000000) {
  for (move in moves) {
    move <- unlist(strsplit(move, ""))

    if (move[1] == "s") {
      x <- as.numeric(paste0(move[-1], collapse = ""))
      programs <- c(programs[(length(programs) - x + 1):length(programs)], programs[1:(length(programs) - x)])
    } else if (move[1] == "x") {
      positions <- as.numeric(unlist(strsplit(paste0(move[-1], collapse = ""), "/")))
      A <- positions[1] + 1
      B <- positions[2] + 1
      temp <- programs[A]
      programs[A] <- programs[B]
      programs[B] <- temp
    } else if (move[1] == "p") {
      positions <- unlist(strsplit(paste0(move[-1], collapse = ""), "/"))
      A <- which(programs == positions[1])
      B <- which(programs == positions[2])
      temp <- programs[A]
      programs[A] <- programs[B]
      programs[B] <- temp
    }
  }

  if (paste(programs, collapse = "") == paste(initial, collapse = "")) {
    cycleLen <- i
    break
  }
}

programs <- initial

for (i in 1:(1000000000 %% cycleLen)) {
  for (move in moves) {
    move <- unlist(strsplit(move, ""))

    if (move[1] == "s") {
      x <- as.numeric(paste0(move[-1], collapse = ""))
      programs <- c(programs[(length(programs) - x + 1):length(programs)], programs[1:(length(programs) - x)])
    } else if (move[1] == "x") {
      positions <- as.numeric(unlist(strsplit(paste0(move[-1], collapse = ""), "/")))
      A <- positions[1] + 1
      B <- positions[2] + 1
      temp <- programs[A]
      programs[A] <- programs[B]
      programs[B] <- temp
    } else if (move[1] == "p") {
      positions <- unlist(strsplit(paste0(move[-1], collapse = ""), "/"))
      A <- which(programs == positions[1])
      B <- which(programs == positions[2])
      temp <- programs[A]
      programs[A] <- programs[B]
      programs[B] <- temp
    }
  }
}

cat(paste(programs, collapse = ""))
