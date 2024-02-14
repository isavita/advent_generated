
input <- readLines("input.txt")
horizontal <- 0
depth <- 0
aim <- 0

for (line in input) {
  parts <- strsplit(line, " ")[[1]]
  command <- parts[1]
  value <- as.numeric(parts[2])
  
  if (command == "forward") {
    horizontal <- horizontal + value
    depth <- depth + (aim * value)
  } else if (command == "down") {
    aim <- aim + value
  } else if (command == "up") {
    aim <- aim - value
  }
}

result <- horizontal * depth
print(result)
