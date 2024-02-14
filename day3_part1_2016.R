
input <- read.table("input.txt", header = FALSE)
possible_triangles <- sum(apply(input, 1, function(x) {
  x <- sort(x)
  x[1] + x[2] > x[3]
}))
print(possible_triangles)
