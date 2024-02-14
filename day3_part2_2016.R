
numbers <- as.matrix(read.table("input.txt"))

isValidTriangle <- function(a, b, c) {
  return (a + b > c && a + c > b && b + c > a)
}

validTriangles <- 0
for (i in 1:ncol(numbers)) {
  for (j in seq(1, nrow(numbers), by=3)) {
    if (j + 2 <= nrow(numbers) && isValidTriangle(numbers[j, i], numbers[j + 1, i], numbers[j + 2, i])) {
      validTriangles <- validTriangles + 1
    }
  }
}

cat(validTriangles, "\n")
