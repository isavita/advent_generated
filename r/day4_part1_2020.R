
# Function to check if a passport is valid
is_valid_passport <- function(passport) {
  required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  fields <- strsplit(passport, "[ \n]")[[1]]
  keys <- sapply(strsplit(fields, ":"), `[`, 1)
  all(required_fields %in% keys)
}

# Main function to process the input and count valid passports
main <- function() {
  # Read the input file
  input <- readLines("input.txt")
  
  # Concatenate lines and split by double newline to get individual passports
  passports <- strsplit(paste(input, collapse = "\n"), "\n\n")[[1]]
  
  # Initialize valid passport count
  valid_count <- 0
  
  # Iterate through each passport and check its validity
  for (passport in passports) {
    if (is_valid_passport(passport)) {
      valid_count <- valid_count + 1
    }
  }
  
  # Print the number of valid passports
  cat(valid_count, "\n")
}

# Run the main function
if (sys.nframe() == 0) {
    main()
}
