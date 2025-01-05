
input <- readLines("input.txt")
moons <- lapply(input, function(line) {
  coords <- as.integer(gsub("[^0-9-]", "", strsplit(line, ",")[[1]]))
  list(pos = coords, vel = c(0, 0, 0))
})

apply_gravity <- function(moons) {
  n <- length(moons)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      for (k in 1:3) {
        if (moons[[i]]$pos[k] > moons[[j]]$pos[k]) {
          moons[[i]]$vel[k] <- moons[[i]]$vel[k] - 1
          moons[[j]]$vel[k] <- moons[[j]]$vel[k] + 1
        } else if (moons[[i]]$pos[k] < moons[[j]]$pos[k]) {
          moons[[i]]$vel[k] <- moons[[i]]$vel[k] + 1
          moons[[j]]$vel[k] <- moons[[j]]$vel[k] - 1
        }
      }
    }
  }
  moons
}

apply_velocity <- function(moons) {
  for (i in 1:length(moons)) {
    moons[[i]]$pos <- moons[[i]]$pos + moons[[i]]$vel
  }
  moons
}

total_energy <- function(moons) {
  sum(sapply(moons, function(m) {
    sum(abs(m$pos)) * sum(abs(m$vel))
  }))
}

for (step in 1:1000) {
  moons <- apply_gravity(moons)
  moons <- apply_velocity(moons)
}

cat(total_energy(moons), "\n")
