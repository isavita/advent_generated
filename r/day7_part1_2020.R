contains <- list()

lines <- readLines("input.txt")
for (line in lines) {
  parts <- strsplit(line, " bags contain ")[[1]]
  container <- parts[1]
  if (parts[2] == "no other bags.") next
  containedBags <- strsplit(parts[2], ", ")[[1]]
  for (bag in containedBags) {
    bagName <- paste(unlist(strsplit(bag, " "))[2:3], collapse = " ")
    if (!is.null(contains[[bagName]])) {
      contains[[bagName]] <- c(contains[[bagName]], container)
    } else {
      contains[[bagName]] <- container
    }
  }
}

countCanContain <- function(target, contains) {
  seen <- c()
  dfs <- function(bag) {
    if (bag %in% names(contains)) {
      for (outer in contains[[bag]]) {
        if (!(outer %in% seen)) {
          seen <<- c(seen, outer)
          dfs(outer)
        }
      }
    }
  }
  dfs(target)
  length(seen)
}

count <- countCanContain("shiny gold", contains)
print(count)