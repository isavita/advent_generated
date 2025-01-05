
data <- scan("input.txt", what = character(), quiet = TRUE)
imageData <- unlist(strsplit(data, ""))
width <- 25
height <- 6
layerSize <- width * height
finalImage <- rep("2", layerSize)

for (i in seq(1, length(imageData), by = layerSize)) {
  layer <- imageData[i:min(i + layerSize - 1, length(imageData))]
  for (j in seq_along(layer)) {
    if (finalImage[j] == "2") {
      finalImage[j] <- layer[j]
    }
  }
}

cat("Decoded image:\n")
for (i in 1:height) {
  for (j in 1:width) {
    pixel <- finalImage[(i - 1) * width + j]
    if (pixel == "0") {
      cat(" ")
    } else if (pixel == "1") {
      cat("#")
    }
  }
  cat("\n")
}
