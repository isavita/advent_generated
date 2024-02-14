
input <- readLines("input.txt")
moves <- strsplit(input, ",")[[1]]

programs <- letters[1:16]

for (move in moves) {
  if (substring(move, 1, 1) == "s") {
    spin <- as.numeric(substring(move, 2))
    programs <- c(tail(programs, spin), head(programs, 16 - spin))
  } else if (substring(move, 1, 1) == "x") {
    positions <- as.numeric(strsplit(substring(move, 2), "/")[[1]])
    temp <- programs[positions[1] + 1]
    programs[positions[1] + 1] <- programs[positions[2] + 1]
    programs[positions[2] + 1] <- temp
  } else if (substring(move, 1, 1) == "p") {
    partners <- strsplit(substring(move, 2), "/")[[1]]
    pos1 <- which(programs == partners[1])
    pos2 <- which(programs == partners[2])
    temp <- programs[pos1]
    programs[pos1] <- programs[pos2]
    programs[pos2] <- temp
  }
}

cat(paste(programs, collapse = ""))
