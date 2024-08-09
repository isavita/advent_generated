bots <- list()

process_line <- function(line) {
  if (grepl("value (\\d+) goes to (bot \\d+)", line)) {
    matches <- regmatches(line, regexec("value (\\d+) goes to (bot \\d+)", line))
    value <- as.integer(matches[[1]][2])
    botID <- matches[[1]][3]
    if (!exists(botID, where = bots)) bots[[botID]] <<- list(chips = integer(0))
    bots[[botID]]$chips <- c(bots[[botID]]$chips, value)
  } else if (grepl("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)", line)) {
    matches <- regmatches(line, regexec("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)", line))
    botID <- matches[[1]][2]
    lowTo <- matches[[1]][3]
    highTo <- matches[[1]][4]
    if (!exists(botID, where = bots)) bots[[botID]] <<- list(chips = integer(0))
    bots[[botID]]$lowTo <- lowTo
    bots[[botID]]$highTo <- highTo
  }
}

give_chip <- function(target, value) {
  if (!exists(target, where = bots)) bots[[target]] <<- list(chips = integer(0))
  bots[[target]]$chips <- c(bots[[target]]$chips, value)
}

input_lines <- readLines("input.txt")
lapply(input_lines, process_line)

while (TRUE) {
  action <- FALSE
  for (botID in names(bots)) {
    if (length(bots[[botID]]$chips) == 2) {
      action <- TRUE
      low <- min(bots[[botID]]$chips)
      high <- max(bots[[botID]]$chips)
      if (low == 17 && high == 61) {
        print(botID)
        quit()
      }
      bots[[botID]]$chips <- integer(0)
      give_chip(bots[[botID]]$lowTo, low)
      give_chip(bots[[botID]]$highTo, high)
    }
  }
  if (!action) break
}