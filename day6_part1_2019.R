
data <- readLines("input.txt")

orbit_map <- strsplit(data, "\\)")

orbit_dict <- list()
for (orbit_pair in orbit_map) {
  parent <- orbit_pair[1]
  child <- orbit_pair[2]
  orbit_dict[[child]] <- parent
}

get_num_orbits <- function(object) {
  if (object == "COM") {
    return(0)
  } else {
    return(1 + get_num_orbits(orbit_dict[[object]]))
  }
}

total_orbits <- sum(sapply(names(orbit_dict), get_num_orbits))
print(total_orbits)
