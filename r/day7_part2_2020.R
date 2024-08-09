parse_rules <- function(lines) {
  rules <- list()
  for (line in lines) {
    parts <- unlist(strsplit(line, " bags contain "))
    outer_bag <- parts[1]
    inner_bags <- gsub(" bags?\\.?$", "", unlist(strsplit(parts[2], ", ")))
    rules[[outer_bag]] <- lapply(inner_bags, function(bag) {
      if (bag == "no other") return(NULL)
      count <- as.numeric(sub(" .*", "", bag))
      color <- sub("^[0-9]+ ", "", bag)
      list(color = color, count = count)
    })
  }
  rules
}

can_contain_gold <- function(rules) {
  cache <<- list()
  contains_gold <- function(bag) {
    if (bag %in% cache) return(cache[[bag]])
    if (is.null(rules[[bag]])) return(FALSE)
    for (inner in rules[[bag]]) {
      if (is.null(inner) || inner$color == "shiny gold" || contains_gold(inner$color)) {
        cache[[bag]] <<- TRUE
        return(TRUE)
      }
    }
    cache[[bag]] <<- FALSE
    FALSE
  }
  sum(sapply(names(rules), contains_gold)) - 1
}

count_bags_inside <- function(rules, bag) {
  if (is.null(rules[[bag]])) return(0)
  sum(sapply(rules[[bag]], function(inner) {
    if (is.null(inner)) return(0)
    inner$count + inner$count * count_bags_inside(rules, inner$color)
  }))
}

main <- function() {
  lines <- readLines("input.txt", warn = FALSE)
  if (length(lines) == 0) stop("Input file is empty.")
  rules <- parse_rules(lines)
  
  part1_result <- can_contain_gold(rules)
  cat("Part 1:", part1_result, "\n")
  
  part2_result <- count_bags_inside(rules, "shiny gold")
  cat("Part 2:", part2_result, "\n")
}

main()