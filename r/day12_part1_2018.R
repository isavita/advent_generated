
input <- readLines("input.txt")
initialState <- strsplit(gsub("initial state: ", "", input[grepl("initial state", input)]), "")[[1]]
rules <- input[grepl("=>", input)]
rules <- setNames(substring(rules, nchar(rules), nchar(rules)), substring(rules, 1, 5))
state <- which(initialState == "#") - 1

for (generation in 1:20) {
  minPot <- min(state)
  maxPot <- max(state)
  newState <- integer(0)
  for (i in (minPot - 2):(maxPot + 2)) {
    pattern <- paste0(ifelse((i - 2):(i + 2) %in% state, "#", "."), collapse = "")
    if (pattern %in% names(rules) && rules[pattern] == "#") {
      newState <- c(newState, i)
    }
  }
  state <- newState
}

print(sum(state))
