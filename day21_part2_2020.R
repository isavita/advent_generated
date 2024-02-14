

data <- readLines("input.txt")

allergen_map <- list()
ingredient_count <- list()

for (line in data) {
  parts <- strsplit(line, " \\(contains ")[[1]]
  ingredients <- strsplit(parts[1], " ")[[1]]
  allergens <- strsplit(gsub("\\)", "", parts[2]), ", ")[[1]]
  
  for (allergen in allergens) {
    if (allergen %in% names(allergen_map)) {
      allergen_map[[allergen]] <- intersect(allergen_map[[allergen]], ingredients)
    } else {
      allergen_map[[allergen]] <- ingredients
    }
  }
  
  for (ingredient in ingredients) {
    if (ingredient %in% names(ingredient_count)) {
      ingredient_count[[ingredient]] <- ingredient_count[[ingredient]] + 1
    } else {
      ingredient_count[[ingredient]] <- 1
    }
  }
}

safe_ingredients <- setdiff(names(ingredient_count), unlist(allergen_map))
answer_part1 <- sum(sapply(safe_ingredients, function(x) ingredient_count[[x]]))

while (any(sapply(allergen_map, length) > 1)) {
  for (allergen in names(allergen_map)) {
    if (length(allergen_map[[allergen]]) == 1) {
      for (other_allergen in names(allergen_map)) {
        if (other_allergen != allergen) {
          allergen_map[[other_allergen]] <- setdiff(allergen_map[[other_allergen]], allergen_map[[allergen]])
        }
      }
    }
  }
}

dangerous_ingredients <- sapply(sort(names(allergen_map)), function(x) allergen_map[[x]])
answer_part2 <- paste(unlist(dangerous_ingredients), collapse = ",")

print(answer_part1)
print(answer_part2)

