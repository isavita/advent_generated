parse_chemical <- function(s) {
  parts <- strsplit(s, " ")[[1]]
  list(name = parts[2], amount = as.numeric(parts[1]))
}

calculate_ore <- function(chem, amount, reactions, ingredients, surplus) {
  if (chem == "ORE") return(amount)
  if (!is.null(surplus[[chem]]) && surplus[[chem]] >= amount) {
    surplus[[chem]] <- surplus[[chem]] - amount
    return(0)
  }
  
  amount <- amount - (ifelse(is.null(surplus[[chem]]), 0, surplus[[chem]]))
  if (!is.null(surplus[[chem]])) surplus[[chem]] <- 0
  
  reaction <- reactions[[chem]]
  times <- ceiling(amount / reaction$amount)
  ore <- 0
  
  for (ingredient in ingredients[[chem]]) {
    ore <- ore + calculate_ore(ingredient$name, ingredient$amount * times, reactions, ingredients, surplus)
  }
  
  surplus[[chem]] <- (ifelse(is.null(surplus[[chem]]), 0, surplus[[chem]])) + times * reaction$amount - amount
  return(ore)
}

max_fuel <- function(reactions, ingredients, ore_available) {
  low <- 0
  high <- ore_available
  while (low < high) {
    mid <- ceiling((low + high + 1) / 2)
    if (calculate_ore("FUEL", mid, reactions, ingredients, new.env()) > ore_available) {
      high <- mid - 1
    } else {
      low <- mid
    }
  }
  low
}

main <- function() {
  reactions <- new.env()
  ingredients <- new.env()
  
  lines <- readLines("input.txt", warn = FALSE)
  for (line in lines) {
    parts <- strsplit(line, " => ")[[1]]
    output <- parse_chemical(parts[2])
    inputs <- strsplit(parts[1], ", ")[[1]]
    ingredients[[output$name]] <- lapply(inputs, parse_chemical)
    reactions[[output$name]] <- output
  }
  
  ore_available <- 1000000000000
  print(max_fuel(reactions, ingredients, ore_available))
}

main()