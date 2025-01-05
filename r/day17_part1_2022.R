
rockstr <- "####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##"

read_all <- function(path) {
  trimws(paste(readLines(path), collapse = ""))
}

get_rocks <- function() {
  rocks <- strsplit(rockstr, "\n\n")[[1]]
  lapply(rocks, function(rock) {
    lines <- strsplit(rock, "\n")[[1]]
    rock_map <- list()
    for (y in seq_along(lines)) {
      line <- strsplit(lines[y], "")[[1]]
      for (x in seq_along(line)) {
        if (line[x] == "#") {
          rock_map[[paste(x - 1, length(lines) - y, sep = ",")]] <- TRUE
        }
      }
    }
    rock_map
  })
}

collision <- function(grid, rock, pos) {
  for (p_str in names(rock)) {
    p <- as.integer(strsplit(p_str, ",")[[1]])
    new_pos <- p + pos
    if (paste(new_pos[1], new_pos[2], sep = ",") %in% names(grid) || new_pos[1] < 0 || new_pos[1] > 6) {
      return(TRUE)
    }
  }
  FALSE
}

dir_from_byte <- function(b) {
  switch(b,
         "N" = c(0, 1),
         "E" = c(1, 0),
         "S" = c(0, -1),
         "W" = c(-1, 0),
         "U" = c(0, 1),
         "R" = c(1, 0),
         "D" = c(0, -1),
         "L" = c(-1, 0),
         "^" = c(0, 1),
         ">" = c(1, 0),
         "v" = c(0, -1),
         "<" = c(-1, 0)
  )
}

main <- function() {
  jet_pattern <- strsplit(read_all("input.txt"), "")[[1]]
  rocks <- get_rocks()
  grid <- list()
  for (x in 0:6) {
    grid[[paste(x, 0, sep = ",")]] <- TRUE
  }
  floor <- 0
  j <- 1
  repeat_map <- list()
  
  i <- 0
  curr <- 0
  while(TRUE){
    if (i == 2022) {
      print(floor)
      break
    }
    key <- list(curr, j)
    key_str <- paste(curr,j,sep=",")
    if(key_str %in% names(repeat_map)){
      repeat_val <- repeat_map[[key_str]]
      
    }else{
      repeat_map[[key_str]] <- list(i,floor)
    }
    
    curr_rock <- rocks[[curr + 1]]
    pos <- c(2, floor + 4)
    while (TRUE) {
      jet <- jet_pattern[j]
      j <- (j %% length(jet_pattern)) + 1
      pos <- pos + dir_from_byte(jet)
      if (collision(grid, curr_rock, pos)) {
        pos <- pos - dir_from_byte(jet)
      }
      pos <- pos + c(0, -1)
      if (collision(grid, curr_rock, pos)) {
        pos <- pos - c(0, -1)
        for (p_str in names(curr_rock)) {
          p <- as.integer(strsplit(p_str, ",")[[1]])
          new_pos <- p + pos
          grid[[paste(new_pos[1], new_pos[2], sep = ",")]] <- TRUE
          if (new_pos[2] > floor) {
            floor <- new_pos[2]
          }
        }
        break
      }
    }
    i <- i + 1
    curr <- (curr + 1) %% length(rocks)
  }
}

main()
