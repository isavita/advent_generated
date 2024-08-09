caves <- list()

connect_to <- function(from, to) {
  if (!from %in% names(caves)) caves[[from]] <<- character(0)
  if (!to %in% names(caves)) caves[[to]] <<- character(0)
  caves[[from]] <<- c(caves[[from]], to)
  caves[[to]] <<- c(caves[[to]], from)
}

count_paths <- function(current, visited) {
  if (current == "end") return(1)
  count <- 0
  for (next_cave in caves[[current]]) {
    if (next_cave %in% visited && tolower(next_cave) == next_cave) next
    count <- count + count_paths(next_cave, c(visited, next_cave))
  }
  count
}

lines <- readLines("input.txt")
for (line in lines) {
  paths <- strsplit(line, "-")[[1]]
  connect_to(paths[1], paths[2])
}

result <- count_paths("start", c("start"))
print(result)