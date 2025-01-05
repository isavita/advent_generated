
read_rules <- function(lines) {
  rules <- list()
  for (line in lines) {
    if (line == "") {
      break
    }
    parts <- strsplit(line, ": ")[[1]]
    rules[[parts[1]]] <- gsub("\"", "", parts[2])
  }
  return(rules)
}

construct_pattern <- function(rules, index) {
  rule <- rules[[as.character(index)]]
  if (grepl("\\|", rule)) {
    subrules <- strsplit(rule, " \\| ")[[1]]
    parts <- sapply(subrules, function(subrule) construct_sub_pattern(rules, subrule))
    return(paste0("(", paste(parts, collapse = "|"), ")"))
  }
  return(construct_sub_pattern(rules, rule))
}

construct_sub_pattern <- function(rules, subrule) {
  if (subrule %in% c("a", "b")) {
    return(subrule)
  }
  sub_idxs <- strsplit(subrule, " ")[[1]]
  pattern <- paste(sapply(sub_idxs, function(idx) construct_pattern(rules, as.integer(idx))), collapse = "")
  return(pattern)
}

count_matches <- function(lines, pattern) {
  re <- paste0("^", pattern, "$")
  sum(grepl(re, lines))
}

main <- function() {
  lines <- readLines("input.txt")
  rules <- read_rules(lines)
  pattern <- construct_pattern(rules, 0)
  messages <- lines[ (which(lines == "")[1]+1):length(lines)]
  count <- count_matches(messages, pattern)
  cat("The number of messages that completely match rule 0 is:", count, "\n")
}

main()
