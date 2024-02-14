
input <- readLines("input.txt")
slope <- 3
trees <- 0
pos <- 0

for (i in 1:length(input)) {
  if (substr(input[i], (pos %% nchar(input[i])) + 1, (pos %% nchar(input[i])) + 1) == "#") {
    trees <- trees + 1
  }
  pos <- pos + slope
}

print(trees)
