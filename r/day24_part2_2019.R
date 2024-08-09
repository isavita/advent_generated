Side <- 5
Square <- Side * Side

parse <- function() {
  res <- logical(Square)
  lines <- readLines("input.txt")
  for (row in seq_along(lines)) {
    for (col in seq_len(Side)) {
      res[(row - 1) * Side + col] <- substr(lines[row], col, col) == "#"
    }
  }
  return(res)
}

next2 <- function(space) {
  newSpace <- list()
  levels <- range(as.integer(names(space)))
  
  for (level in (levels[1] - 1):(levels[2] + 1)) {
    newSpace[[as.character(level)]] <- logical(Square)
    
    for (cell in 0:(Square - 1)) {
      if (cell == 12) next
      
      neighbours <- 0
      row <- cell %/% Side
      col <- cell %% Side
      
      if (row == 0 && infested(space, level - 1, 7)) neighbours <- neighbours + 1
      if (col == 0 && infested(space, level - 1, 11)) neighbours <- neighbours + 1
      if (col == 4 && infested(space, level - 1, 13)) neighbours <- neighbours + 1
      if (row == 4 && infested(space, level - 1, 17)) neighbours <- neighbours + 1
      
      if (cell == 7) for (i in 0:(Side - 1)) neighbours <- neighbours + infested(space, level + 1, i)
      if (cell == 11) for (i in 0:4) neighbours <- neighbours + infested(space, level + 1, 5 * i)
      if (cell == 13) for (i in 0:4) neighbours <- neighbours + infested(space, level + 1, 5 * i + Side - 1)
      if (cell == 17) for (i in 0:4) neighbours <- neighbours + infested(space, level + 1, (Side - 1) * Side + i)
      
      if (row > 0 && cell != 17) neighbours <- neighbours + infested(space, level, cell - Side)
      if (col > 0 && cell != 13) neighbours <- neighbours + infested(space, level, cell - 1)
      if (col < Side - 1 && cell != 11) neighbours <- neighbours + infested(space, level, cell + 1)
      if (row < Side - 1 && cell != 7) neighbours <- neighbours + infested(space, level, cell + Side)
      
      newSpace[[as.character(level)]][cell + 1] <- ifelse(infested(space, level, cell) && neighbours != 1, FALSE,
                                       ifelse(!infested(space, level, cell) && (neighbours == 1 || neighbours == 2), TRUE,
                                       infested(space, level, cell)))
    }
  }
  
  clean(newSpace)
  return(newSpace)
}

clean <- function(space) {
  minMax <- range(as.integer(names(space)))
  for (level in minMax) {
    if (sum(space[[as.character(level)]]) == 0) {
      space[[as.character(level)]] <- NULL
    }
  }
}

infested <- function(space, level, cell) {
  if (is.null(space[[as.character(level)]])) return(FALSE)
  return(space[[as.character(level)]][cell + 1])
}

countInfested <- function(space) {
  sum(sapply(space, sum))
}

main <- function() {
  input <- parse()
  space <- list(`0` = input)
  
  for (i in 1:200) {
    space <- next2(space)
  }
  
  print(countInfested(space))
}

main()