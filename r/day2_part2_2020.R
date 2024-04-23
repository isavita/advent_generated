validate_password <- function(policy, password) {
  policy_parts <- strsplit(policy, "[- ]")[[1]]
  min <- as.integer(policy_parts[1])
  max <- as.integer(policy_parts[2])
  char <- strsplit(policy_parts[3], "")[[1]][1]
  
  password_chars <- strsplit(password, "")[[1]]
  
  char_count <- sum(password_chars == char)
  
  return ((password_chars[min] == char) != (password_chars[max] == char))
}

valid_count <- 0

file_lines <- readLines("input.txt")

for (line in file_lines) {
  parts <- strsplit(line, ": ")[[1]]
  policy <- parts[1]
  password <- parts[2]
  
  if (validate_password(policy, password)) {
    valid_count <- valid_count + 1
  }
}

print(valid_count)