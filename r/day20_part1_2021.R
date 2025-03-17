
# Function to convert a 3x3 neighborhood to an index
neighborhood_to_index <- function(image, row, col, default_val) {
    index_str <- ""
    for (i in (row - 1):(row + 1)) {
        for (j in (col - 1):(col + 1)) {
            if (i >= 1 && i <= nrow(image) && j >= 1 && j <= ncol(image)) {
                index_str <- paste0(index_str, ifelse(image[i, j] == "#", "1", "0"))
            } else {
                index_str <- paste0(index_str, ifelse(default_val == "#", "1", "0"))
            }
        }
    }
    return(strtoi(index_str, base = 2))
}

# Function to enhance the image
enhance_image <- function(image, algorithm, default_val) {
    new_rows <- nrow(image) + 2
    new_cols <- ncol(image) + 2
    new_image <- matrix(default_val, nrow = new_rows, ncol = new_cols)

    for (i in 1:new_rows) {
        for (j in 1:new_cols) {
            index <- neighborhood_to_index(image, i - 1, j - 1, default_val)
            new_image[i, j] <- substr(algorithm, index + 1, index + 1)
        }
    }
    return(new_image)
}

# Main function
main <- function() {
    # Read input from file
    input <- readLines("input.txt")
    
    # Parse algorithm and image
    algorithm <- input[1]
    image_lines <- input[3:length(input)]
    image_rows <- length(image_lines)
    image_cols <- nchar(image_lines[1])
    image <- matrix("", nrow = image_rows, ncol = image_cols)
    for (i in 1:image_rows) {
        image[i, ] <- strsplit(image_lines[i], "")[[1]]
    }
    
    # Determine the default value for infinite pixels
    default_val <- "." # Initialize with "." for the first enhancement
    
    # Enhance the image twice
    for (k in 1:2) {
       image <- enhance_image(image, algorithm, default_val)
        # Update default_val based on algorithm, important for part 1 and 2 differences
        if (default_val == ".") {
          default_val <- substr(algorithm, 1, 1)
        } else {
          default_val <- substr(algorithm, nchar(algorithm), nchar(algorithm))
        }

    }

    # Count lit pixels
    lit_pixels <- sum(image == "#")
    cat(lit_pixels, "\n")
}

# Set working directory to script's location (only when run interactively, not sourced)
if (interactive()) {
   script_path <- dirname(parent.frame(2)$ofile)
   if(!is.null(script_path)){
        if (script_path != "") setwd(script_path)
   }
}

# Run the main function
main()
