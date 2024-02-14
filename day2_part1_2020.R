

# Read input from file
input <- readLines("input.txt", warn = FALSE)

# Function to check if password is valid
is_valid_password <- function(policy, password) {
  parts <- strsplit(policy, " ")[[1]]
  range <- as.numeric(strsplit(parts[1], "-")[[1]])
  letter <- strsplit(parts[2], ":")[[1]]
  
  count <- nchar(gsub(paste0("[^", letter, "]"), "", password))
  
  return(count >= range[1] && count <= range[2])
}

# Count valid passwords
valid_count <- 0
for (line in input) {
  parts <- strsplit(line, ": ")[[1]]
  if (is_valid_password(parts[1], parts[2])) {
    valid_count <- valid_count + 1
  }
}

# Print answer
print(valid_count)

