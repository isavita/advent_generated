
read_ingredients <- function(filename) {
  lines <- readLines(filename)
  ingredients <- list()
  for (line in lines) {
    parts <- strsplit(line, " ")[[1]]
    if (length(parts) < 11) {
      next
    }
    capacity <- as.integer(gsub(",", "", parts[3]))
    durability <- as.integer(gsub(",", "", parts[5]))
    flavor <- as.integer(gsub(",", "", parts[7]))
    texture <- as.integer(gsub(",", "", parts[9]))
    calories <- as.integer(parts[11])
    ingredients[[parts[1]]] <- list(
      capacity = capacity,
      durability = durability,
      flavor = flavor,
      texture = texture,
      calories = calories
    )
  }
  return(ingredients)
}

calculate_calories <- function(ingredients, teaspoons) {
  calories <- 0
  for (i in seq_along(ingredients)) {
    calories <- calories + ingredients[[i]]$calories * teaspoons[i]
  }
  return(calories)
}

score <- function(ingredients, teaspoons) {
  capacity <- 0
  durability <- 0
  flavor <- 0
  texture <- 0
  for (i in seq_along(ingredients)) {
    capacity <- capacity + ingredients[[i]]$capacity * teaspoons[i]
    durability <- durability + ingredients[[i]]$durability * teaspoons[i]
    flavor <- flavor + ingredients[[i]]$flavor * teaspoons[i]
    texture <- texture + ingredients[[i]]$texture * teaspoons[i]
  }
  capacity <- max(0, capacity)
  durability <- max(0, durability)
  flavor <- max(0, flavor)
  texture <- max(0, texture)
  return(capacity * durability * flavor * texture)
}

calculate_max_score <- function(ingredients, index, remaining, teaspoons, target_calories) {
  if (index == length(ingredients)) {
    teaspoons[index] <- remaining
    if (calculate_calories(ingredients, teaspoons) == target_calories) {
      return(score(ingredients, teaspoons))
    }
    return(0)
  }
  max_score_val <- 0
  for (i in 0:remaining) {
    teaspoons[index] <- i
    current_score <- calculate_max_score(ingredients, index + 1, remaining - i, teaspoons, target_calories)
    max_score_val <- max(max_score_val, current_score)
  }
  return(max_score_val)
}

find_max_score <- function(ingredients, total_teaspoons, target_calories) {
  return(calculate_max_score(ingredients, 1, total_teaspoons, integer(length(ingredients)), target_calories))
}

main <- function() {
  ingredients <- read_ingredients("input.txt")
  max_score_result <- find_max_score(ingredients, 100, 500)
  cat(max_score_result)
}

main()
