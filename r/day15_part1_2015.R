
# Function to parse ingredient properties from a line
parse_ingredient <- function(line) {
  parts <- unlist(strsplit(line, "[,:]?\\s+"))
  name <- parts[1]
  properties <- as.numeric(parts[c(3, 5, 7, 9, 11)])
  names(properties) <- c("capacity", "durability", "flavor", "texture", "calories")
  return(list(name = name, properties = properties))
}

# Function to calculate the score of a cookie
calculate_score <- function(ingredients, amounts) {
  num_ingredients <- length(ingredients)
  totals <- numeric(4)  # capacity, durability, flavor, texture
  
  for (i in 1:num_ingredients) {
    totals <- totals + amounts[i] * ingredients[[i]]$properties[1:4]
  }
  
  totals[totals < 0] <- 0
  return(prod(totals))
}

# Function to find best combination (optimized with constraints)
find_best_combination <- function(ingredients, total_teaspoons = 100) {
    
    num_ingredients <- length(ingredients)
    best_score <- 0
    
    if (num_ingredients == 2) {
        # Two ingredients are simple to optimize.
        for (i in 0:total_teaspoons) {
            amounts <- c(i, total_teaspoons - i)
            score <- calculate_score(ingredients, amounts)
            best_score <- max(best_score, score)
        }
    } else if (num_ingredients == 3){
        for(i in 0:total_teaspoons){
            for(j in 0:(total_teaspoons-i)){
                k <- total_teaspoons - i - j
                amounts <- c(i,j,k)
                score <- calculate_score(ingredients, amounts)
                best_score <- max(best_score, score)
            }
        }
    }
    else if (num_ingredients == 4){ # Optimized recursion for four ingredients.
       for(i in 0:total_teaspoons){
           for(j in 0:(total_teaspoons - i)){
               for(k in 0:(total_teaspoons - i - j)){
                    l <- total_teaspoons - i - j - k;
                    amounts <- c(i,j,k,l)
                    score <- calculate_score(ingredients, amounts)
                    best_score <- max(best_score, score)
               }
           }
       }
    }
    else{
      stop("This solver is optimized only up to 4 ingredients.")
    }    
    return(best_score)
}


# Main function
main <- function() {
  # Read input from file
  lines <- readLines("input.txt")
  
  # Parse ingredients
  ingredients <- lapply(lines, parse_ingredient)
  
  # Find the best combination and score
  best_score <- find_best_combination(ingredients)
  
  # Print the result
  cat(best_score, "\n")
}

# Run the main function
main()
