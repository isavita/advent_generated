read_input <- function(filename) {
  readLines(filename, n = 1)
}

increment_password <- function(password) {
  chars <- strsplit(password, "")[[1]]
  for (i in length(chars):1) {
    chars[i] <- ifelse(chars[i] == "z", "a", intToUtf8(utf8ToInt(chars[i]) + 1))
    if (chars[i] != "a") break
  }
  paste(chars, collapse = "")
}

is_valid_password <- function(password) {
  has_straight(password) && !contains_invalid_letters(password) && has_two_pairs(password)
}

has_straight <- function(password) {
  for (i in 1:(nchar(password) - 2)) {
    if (utf8ToInt(substr(password, i, i)) + 1 == utf8ToInt(substr(password, i + 1, i + 1)) &&
        utf8ToInt(substr(password, i, i)) + 2 == utf8ToInt(substr(password, i + 2, i + 2))) {
      return(TRUE)
    }
  }
  FALSE
}

contains_invalid_letters <- function(password) {
  any(strsplit(password, "")[[1]] %in% c("i", "o", "l"))
}

has_two_pairs <- function(password) {
  count <- 0
  i <- 1
  while (i < nchar(password)) {
    if (substr(password, i, i) == substr(password, i + 1, i + 1)) {
      count <- count + 1
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  count >= 2
}

current_password <- read_input("input.txt")
first_new_password <- increment_password(current_password)
while (!is_valid_password(first_new_password)) {
  first_new_password <- increment_password(first_new_password)
}
second_new_password <- increment_password(first_new_password)
while (!is_valid_password(second_new_password)) {
  second_new_password <- increment_password(second_new_password)
}
cat(second_new_password, "\n")