
reg_split <- function(text, delimiter) {
  return(strsplit(text, delimiter)[[1]])
}

str_to_int <- function(s) {
  return(as.integer(s))
}

main <- function() {
  input_str <- readChar("input.txt", file.info("input.txt")$size)
  input_str <- trimws(input_str)
  lines <- reg_split(input_str, "\n")
  
  ground <- matrix("+", nrow = 1, ncol = 1)
  maxX <- 0
  minX <- 0
  maxY <- 0
  minY <- 20
  xOffset <- 500
  yOffset <- 0
  
  for (line in lines) {
    split <- reg_split(line, "[=, .]+")
    if (split[1] == "x") {
      x <- str_to_int(split[2]) - xOffset
      y1 <- str_to_int(split[4]) - yOffset
      y2 <- str_to_int(split[5]) - yOffset
      
      while (x >= maxX) {
        maxX <- maxX + 1
        ground <- cbind(ground, rep(".", nrow(ground)))
      }
      while (x <= minX) {
        minX <- minX - 1
        ground <- cbind(rep(".", nrow(ground)), ground)
      }
      while (y2 > maxY) {
        maxY <- maxY + 1
        ground <- rbind(ground, rep(".", ncol(ground)))
      }
      minY <- min(minY, y1)
      ground[(y1 + 1):(y2 + 1), x - minX + 1] <- "#"
      
    } else {
      y <- str_to_int(split[2]) - yOffset
      x1 <- str_to_int(split[4]) - xOffset
      x2 <- str_to_int(split[5]) - xOffset
      
      while (y > maxY) {
        maxY <- maxY + 1
        ground <- rbind(ground, rep(".", ncol(ground)))
      }
      while (x2 >= maxX) {
        maxX <- maxX + 1
        ground <- cbind(ground, rep(".", nrow(ground)))
      }
      while (x1 <= minX) {
        minX <- minX - 1
        ground <- cbind(rep(".", nrow(ground)), ground)
      }
      minY <- min(minY, y)
      ground[y + 1, (x1 - minX + 1):(x2 - minX + 1)] <- "#"
    }
  }
  
  water_count <- 0
  flow_count <- 0
  round_limit <- 200000
  
  while (ground[2, -minX + 1] != "|" && water_count < round_limit) {
    can_move <- TRUE
    x <- -minX + 1
    y <- 2
    try_left <- 0
    
    while (can_move) {
      if (y + 1 > maxY + 1 || ground[y + 1, x] == "|") {
        ground[y, x] <- "|"
        can_move <- FALSE
        if (y >= minY + 1) {
          flow_count <- flow_count + 1
        }
      } else if (ground[y + 1, x] == ".") {
        y <- y + 1
        try_left <- 0
      } else if (ground[y + 1, x] %in% c("#", "~")) {
        if (((try_left == 1 && ground[y, x - 1] == "|") ||
             (try_left == 2 && ground[y, x + 1] == "|") ||
             (ground[y, x + 1] == "|" && ground[y, x - 1] != ".") ||
             (ground[y, x + 1] != "." && ground[y, x - 1] == "|"))) {
          ground[y, x] <- "|"
          flow_count <- flow_count + 1
          can_move <- FALSE
          for (i in (x + 1):ncol(ground)) {
            if (ground[y, i] == "~") {
              ground[y, i] <- "|"
              water_count <- water_count - 1
              flow_count <- flow_count + 1
            } else {
              break
            }
          }
          for (i in (x - 1):1) {
            if (ground[y, i] == "~") {
              ground[y, i] <- "|"
              water_count <- water_count - 1
              flow_count <- flow_count + 1
            } else {
              break
            }
          }
        } else if (((try_left == 0 && ground[y, x - 1] == ".") ||
                    (try_left == 1 && ground[y, x - 1] == "."))) {
          x <- x - 1
          try_left <- 1
        } else if (((try_left == 0 && ground[y, x + 1] == ".") ||
                    (try_left == 2 && ground[y, x + 1] == "."))) {
          x <- x + 1
          try_left <- 2
        } else {
          can_move <- FALSE
          ground[y, x] <- "~"
          water_count <- water_count + 1
        }
      }
    }
  }
  
  cat(flow_count + water_count)
}

main()
