
polymer <- readLines("input.txt")
reacted_polymer <- Reduce(function(polymer, unit) {
  if(length(polymer) == 0) {
    return(c(unit))
  }
  if(toupper(unit) == toupper(tail(polymer, 1)) & unit != tail(polymer, 1)) {
    return(head(polymer, -1))
  }
  return(c(polymer, unit))
}, unlist(strsplit(polymer, "")), init = numeric(0))

print(length(reacted_polymer))
