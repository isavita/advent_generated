
# Function to rotate a matrix 90 degrees clockwise
rotate <- function(matrix) {
  t(matrix[nrow(matrix):1, ])
}

# Function to flip a matrix horizontally
flip_horizontal <- function(matrix) {
  matrix[, ncol(matrix):1]
}

# Function to flip a matrix vertically (same as rotate twice, then flip_h)
flip_vertical <- function(matrix) {
    matrix[nrow(matrix):1,]
}


# Function to convert a string representation to a matrix
string_to_matrix <- function(s) {
  rows <- strsplit(s, "/")[[1]]
  matrix(unlist(strsplit(rows, "")), nrow = length(rows), byrow = TRUE)
}

# Function to convert a matrix to a string representation
matrix_to_string <- function(matrix) {
  paste(apply(matrix, 1, paste, collapse = ""), collapse = "/")
}

# Function to enhance the image based on rules
enhance <- function(image, rules) {
  size <- nrow(image)
  
  if (size %% 2 == 0) {
    block_size <- 2
    new_block_size <- 3
  } else {
    block_size <- 3
    new_block_size <- 4
  }
  
  num_blocks <- size / block_size
  new_size <- num_blocks * new_block_size
  new_image <- matrix("", nrow = new_size, ncol = new_size)
  
  for (i in 1:num_blocks) {
    for (j in 1:num_blocks) {
      block <- image[((i - 1) * block_size + 1):(i * block_size),
                     ((j - 1) * block_size + 1):(j * block_size)]
      
      block_str <- matrix_to_string(block)
      
      matched <- FALSE
      for (k in 1:length(rules))
      {
          rule_input <- strsplit(names(rules)[k], " => ")[[1]][1]
          
          temp_matrix <- string_to_matrix(rule_input)
          
          for(rot in 1:4)
          {
            if (matrix_to_string(temp_matrix) == block_str) {
                
                replacement <- rules[[k]]
                new_image[((i - 1) * new_block_size + 1):(i * new_block_size),
                          ((j - 1) * new_block_size + 1):(j * new_block_size)] <- string_to_matrix(replacement)
                matched <- TRUE
                break
            }
              
            if (matrix_to_string(flip_horizontal(temp_matrix)) == block_str){
                
                replacement <- rules[[k]]
                new_image[((i - 1) * new_block_size + 1):(i * new_block_size),
                          ((j - 1) * new_block_size + 1):(j * new_block_size)] <- string_to_matrix(replacement)
                matched <- TRUE
                break
            }
              
            if (matrix_to_string(flip_vertical(temp_matrix)) == block_str){

                replacement <- rules[[k]]
                new_image[((i - 1) * new_block_size + 1):(i * new_block_size),
                          ((j - 1) * new_block_size + 1):(j * new_block_size)] <- string_to_matrix(replacement)
                matched <- TRUE
                break
              
            }              
            temp_matrix <- rotate(temp_matrix)
          }
        if(matched) break;
      }
        
    }
  }
  
  return(new_image)
}

main <- function() {
  # Read rules from input file
  input <- readLines("input.txt")
  rules <- list()
  for (line in input) {
    parts <- strsplit(line, " => ")[[1]]
    rules[[parts[1]]] <- parts[2]
  }
  
  # Initial image
  initial_image <- c(".#.", "..#", "###")
  image <- string_to_matrix(paste(initial_image, collapse = "/"))
  
  # Iterate 5 times
  for (i in 1:5) {
    image <- enhance(image, rules)
  }
  
  # Count the number of '#' pixels
  cat(sum(image == "#"), "\n")
}


main()
