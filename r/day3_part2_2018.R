read_claims <- function(filename) {
  claims <- list()
  lines <- readLines(filename)
  for (line in lines) {
    parts <- strsplit(line, " ")[[1]]
    id <- as.integer(sub("#", "", parts[1]))
    coords <- as.integer(unlist(strsplit(sub(":", "", parts[3]), ",")))
    dims <- as.integer(unlist(strsplit(parts[4], "x")))
    claims[[length(claims) + 1]] <- list(ID = id, X = coords[1], Y = coords[2], Width = dims[1], Height = dims[2])
  }
  return(claims)
}

claims <- read_claims("input.txt")
fabric <- matrix(0, nrow = 1000, ncol = 1000)

for (claim in claims) {
  for (y in claim$Y:(claim$Y + claim$Height - 1)) {
    for (x in claim$X:(claim$X + claim$Width - 1)) {
      fabric[y + 1, x + 1] <- fabric[y + 1, x + 1] + 1
    }
  }
}

for (claim in claims) {
  overlap <- FALSE
  for (y in claim$Y:(claim$Y + claim$Height - 1)) {
    for (x in claim$X:(claim$X + claim$Width - 1)) {
      if (fabric[y + 1, x + 1] > 1) {
        overlap <- TRUE
        break
      }
    }
    if (overlap) break
  }
  if (!overlap) {
    print(claim$ID)
    break
  }
}