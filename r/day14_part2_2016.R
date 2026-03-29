
#!/usr/bin/env Rscript

# Day 14: One-Time Pad
# This script calculates the 64th key for both Part 1 (Standard MD5)
# and Part 2 (Key Stretching) of the challenge.

# For efficiency, we use the 'openssl' package.
# Base R does not have a native string-to-MD5 function without using files.
# 'openssl' provides a vectorized MD5 implementation, which is crucial
# for calculating millions of hashes in a reasonable time for Part 2.
if (!requireNamespace("openssl", quietly = TRUE)) {
  stop("The 'openssl' package is required. Please install it with: install.packages('openssl')")
}

#' Finds the index of the 64th key produced by the salt.
#' @param salt The puzzle input string.
#' @param stretch Logical; if TRUE, apply 2016 additional MD5 rounds.
#' @return The integer index of the 64th valid key.
find_key_64 <- function(salt, stretch = FALSE) {
  found_keys <- 0
  i <- 0
  hash_cache <- character()
  batch_size <- 5000 # Process in batches for better vectorization performance

  while (TRUE) {
    # Ensure the cache contains enough hashes for lookahead (current index + 1000)
    # We maintain a sliding window of hashes to avoid redundant calculations.
    if (length(hash_cache) <= (i + 1000)) {
      start_idx <- length(hash_cache)
      end_idx <- start_idx + batch_size - 1
      indices <- start_idx:end_idx
      
      # Step 1: Initialize hashes with Salt + Index
      h <- paste0(salt, indices)
      
      # Step 2: Perform MD5 Hashing
      if (stretch) {
        # Part 2: Key stretching (total 2017 MD5 iterations)
        # Vectorizing this loop ensures high performance in R.
        for (k in 1:2017) {
          h <- as.character(openssl::md5(h))
        }
      } else {
        # Part 1: Single MD5 pass
        h <- as.character(openssl::md5(h))
      }
      
      # Extend the cache with the newly calculated batch
      hash_cache <- c(hash_cache, h)
    }

    # Retrieve current hash (using 1-based indexing for R vectors)
    current_hash <- hash_cache[i + 1]
    
    # Check for a triplet: any character repeating 3 times in a row.
    # The problem specifies we only consider the FIRST triplet.
    m <- regexpr("(.)\\1\\1", current_hash, perl = TRUE)

    if (m != -1) {
      # Extract the triplet character
      triplet_char <- substr(current_hash, m, m)
      
      # Search criteria: the same character must appear 5 times in a row 
      # in any of the next 1000 hashes (i+1 to i+1000).
      quintuplet_pattern <- strrep(triplet_char, 5)
      
      # Lookahead range mapping: 
      # Hash index i corresponds to cache index i+1.
      # Next 1000 hashes correspond to cache indices i+2 to i+1001.
      lookahead_range <- hash_cache[(i + 2):(i + 1001)]
      
      # Efficiently check the lookahead window using grepl with fixed mapping
      if (any(grepl(quintuplet_pattern, lookahead_range, fixed = TRUE))) {
        found_keys <- found_keys + 1
        
        # We need the index of the 64th key
        if (found_keys == 64) {
          return(i)
        }
      }
    }
    
    # Increment index to check the next hash
    i <- i + 1
    
    # Cleanup memory periodically (Optional)
    if (i > 10000 && i %% 5000 == 0) {
      # In R, large vector modifications can be slow, but for the scale 
      # of this problem (~30k indices), keeping the full vector is fine.
    }
  }
}

main <- function() {
  input_file <- "input.txt"
  if (!file.exists(input_file)) {
    cat("Error: input.txt not found in the current directory.\n")
    return()
  }
  
  # Read salt and remove any whitespace
  salt <- trimws(readLines(input_file, warn = FALSE)[1])
  
  if (is.na(salt) || salt == "") {
    cat("Error: Salt input is empty.\n")
    return()
  }

  # Part 1: Standard one-time pad generation
  ans1 <- find_key_64(salt, stretch = FALSE)
  cat(ans1, "\n")
  
  # Part 2: Key stretching generation (2016 additional rounds)
  ans2 <- find_key_64(salt, stretch = TRUE)
  cat(ans2, "\n")
}

# Run the program
main()

