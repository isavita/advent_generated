
solve_chocolate_charts <- function(input_file) {
  input <- as.integer(readLines(input_file))
  
  recipes <- c(3, 7)
  elf1_pos <- 1
  elf2_pos <- 2
  
  while (length(recipes) < input + 10) {
    sum_scores <- recipes[elf1_pos] + recipes[elf2_pos]
    new_recipes <- if (sum_scores >= 10) {
      c(sum_scores %/% 10, sum_scores %% 10)
    } else {
      sum_scores
    }
    recipes <- c(recipes, new_recipes)
    
    elf1_pos <- ((elf1_pos + recipes[elf1_pos] ) %% length(recipes)) + 1
    elf2_pos <- ((elf2_pos + recipes[elf2_pos] ) %% length(recipes)) + 1
  }
  
  result <- paste(recipes[(input + 1):(input + 10)], collapse = "")
  cat(result, "\n")
}

# Example usage with input.txt
solve_chocolate_charts("input.txt")
