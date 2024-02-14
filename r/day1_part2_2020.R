data <- as.numeric(readLines("input.txt"))

# Part One
for (i in 1:length(data)) {
  for (j in i:length(data)) {
    if (data[i] + data[j] == 2020) {
      print(data[i] * data[j])
    }
  }
}

# Part Two
for (i in 1:length(data)) {
  for (j in i:length(data)) {
    for (k in j:length(data)) {
      if (data[i] + data[j] + data[k] == 2020) {
        print(data[i] * data[j] * data[k])
      }
    }
  }
}