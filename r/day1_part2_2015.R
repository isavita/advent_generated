
input <- trimws(readLines("input.txt"))
floor <- 0
position <- 0
for (i in 1:nchar(input)) {
  if (substr(input, i, i) == "(") {
    floor <- floor + 1
  } else if (substr(input, i, i) == ")") {
    floor <- floor - 1
  }
  if (floor == -1) {
    position <- i
    break
  }
}
cat(position, "\n")
