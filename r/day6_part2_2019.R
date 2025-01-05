
# Function to read input from file and parse orbits
read_orbits <- function(filename) {
  lines <- readLines(filename)
  orbits <- strsplit(lines, "\\)")
  names(orbits) <- sapply(orbits, `[`, 2)
  orbits <- lapply(orbits, `[`, 1)
  return(orbits)
}

# Function to calculate total orbits for part 1
calculate_total_orbits <- function(orbits) {
  count_orbits <- function(obj, orbits, depth = 0) {
    if (obj == "COM") {
      return(depth)
    } else {
      parent <- orbits[[obj]]
      return(count_orbits(parent, orbits, depth + 1))
    }
  }
  total_orbits <- sum(sapply(names(orbits), count_orbits, orbits = orbits))
  return(total_orbits)
}

# Function to find path to COM
find_path_to_com <- function(obj, orbits) {
  path <- c()
  while (obj != "COM") {
    path <- c(path, obj)
    obj <- orbits[[obj]]
  }
  return(path)
}

# Function to calculate orbital transfers for part 2
calculate_orbital_transfers <- function(orbits) {
  you_parent <- orbits[["YOU"]]
  san_parent <- orbits[["SAN"]]
  
  you_path <- find_path_to_com(you_parent, orbits)
  san_path <- find_path_to_com(san_parent, orbits)
  
  # Find the common ancestor
  common_ancestor <- ""
  for (i in seq_along(you_path)) {
    if (you_path[i] %in% san_path) {
      common_ancestor <- you_path[i]
      break
    }
  }
  
  # Calculate transfers
  you_transfers <- which(you_path == common_ancestor) - 1
  san_transfers <- which(san_path == common_ancestor) - 1
  
  return(you_transfers + san_transfers)
}

# Main program
orbits <- read_orbits("input.txt")

# Part 1
total_orbits <- calculate_total_orbits(orbits)
cat("Total direct and indirect orbits:", total_orbits, "\n")

# Part 2
orbital_transfers <- calculate_orbital_transfers(orbits)
cat("Minimum orbital transfers:", orbital_transfers, "\n")
