
input <- readLines("input.txt")
totalPower <- 0

for (line in input) {
  matches <- regmatches(line, regexec("Game (\\d+): (.+)", line))
  
  if (length(matches[[1]]) == 3) {
    rounds <- unlist(strsplit(matches[[1]][3], ";"))
    maxRed <- 0
    maxGreen <- 0
    maxBlue <- 0
    
    for (round in rounds) {
      cubes <- regmatches(round, gregexpr("(\\d+) (red|green|blue)", round))[[1]]
      red <- 0
      green <- 0
      blue <- 0
      
      for (cube in cubes) {
        count <- as.numeric(regmatches(cube, gregexpr("\\d+", cube))[[1]])
        color <- regmatches(cube, gregexpr("red|green|blue", cube))[[1]]
        
        if (color == "red") {
          red <- red + count
        } else if (color == "green") {
          green <- green + count
        } else if (color == "blue") {
          blue <- blue + count
        }
      }
      
      if (red > maxRed) {
        maxRed <- red
      }
      if (green > maxGreen) {
        maxGreen <- green
      }
      if (blue > maxBlue) {
        maxBlue <- blue
      }
    }
    
    power <- maxRed * maxGreen * maxBlue
    totalPower <- totalPower + power
  }
}

cat(totalPower, "\n")
