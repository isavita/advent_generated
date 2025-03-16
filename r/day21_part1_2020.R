
# Function to parse the input and extract ingredients and allergens
parse_input <- function(file_path) {
    lines <- readLines(file_path)
    ingredients_list <- list()
    allergens_map <- list()
    
    for (line in lines) {
        parts <- strsplit(line, " \\(contains ")[[1]]
        ingredients <- strsplit(parts[1], " ")[[1]]
        allergens <- strsplit(gsub("\\)", "", parts[2]), ", ")[[1]]
        
        ingredients_list <- c(ingredients_list, list(ingredients))
        
        for (allergen in allergens) {
            if (is.null(allergens_map[[allergen]])) {
                allergens_map[[allergen]] <- ingredients
            } else {
                allergens_map[[allergen]] <- intersect(allergens_map[[allergen]], ingredients)
            }
        }
    }
    
    return(list(ingredients_list = ingredients_list, allergens_map = allergens_map))
}

# Function to identify ingredients that cannot contain any allergens
find_safe_ingredients <- function(ingredients_list, allergens_map) {
  all_ingredients <- unique(unlist(ingredients_list))
  possible_allergen_ingredients <- unique(unlist(allergens_map))
  safe_ingredients <- setdiff(all_ingredients, possible_allergen_ingredients)
  return(safe_ingredients)
}


# Function to count occurrences of safe ingredients
count_safe_ingredients <- function(ingredients_list, safe_ingredients) {
    count <- 0
    for (ingredients in ingredients_list) {
        count <- count + sum(ingredients %in% safe_ingredients)
    }
    return(count)
}

# Main function
main <- function() {
    # Parse input
    parsed_data <- parse_input("input.txt")
    ingredients_list <- parsed_data$ingredients_list
    allergens_map <- parsed_data$allergens_map

    # Find safe ingredients
    safe_ingredients <- find_safe_ingredients(ingredients_list, allergens_map)

    # Count occurrences of safe ingredients
    total_count <- count_safe_ingredients(ingredients_list, safe_ingredients)
    
    cat(total_count, "\n")
}

# Run the main function
main()
