imageData <- readLines("input.txt")
imageData <- gsub("\\s+", "", imageData)

width <- 25
height <- 6
layerSize <- width * height

minZeros <- layerSize + 1
result <- 0

for (i in seq(1, nchar(imageData), by = layerSize)) {
  layer <- substr(imageData, i, i + layerSize - 1)
  counts <- table(strsplit(layer, "")[[1]])
  zeroCount <- counts["0"]
  oneCount <- counts["1"]
  twoCount <- counts["2"]
  
  if (zeroCount < minZeros) {
    minZeros <- zeroCount
    result <- oneCount * twoCount
  }
}

print(result)