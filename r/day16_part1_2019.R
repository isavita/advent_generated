
# Function to calculate the FFT for a single phase
fft_phase <- function(input) {
  len <- length(input)
  output <- numeric(len)
  
  for (i in 1:len) {
    pattern <- rep(c(0, 1, 0, -1), each = i)
    pattern_len <- length(pattern)
    
    # Efficiently repeat and offset the pattern
    total <- 0
    
    if (pattern_len >= len +1 ) {
            total <- sum(input * pattern[2:(len + 1)])  
    } else {
        
        full_pattern <- rep(pattern, ceiling((len + 1) / pattern_len))
        
        total <- sum(input * full_pattern[2:(len + 1)])
    }
     
    output[i] <- abs(total) %% 10
  }
  
  return(output)
}

# Main function to read input, run FFT, and print output
main <- function() {
  # Read input from file
  input_str <- readLines("input.txt")
  input <- as.integer(strsplit(input_str, "")[[1]])
  
  # Run FFT for 100 phases
  for (i in 1:100) {
    input <- fft_phase(input)
  }
  
  # Print the first eight digits
  cat(paste(input[1:8], collapse = ""), "\n")
}

# Run the main function
main()
