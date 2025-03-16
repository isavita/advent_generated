
parse_snail_number <- function(s) {
  if (substr(s, 1, 1) != '[') {
    return(list(value = as.integer(s), left = NULL, right = NULL))
  }
  balance <- 0
  split_index <- 0
  for (i in 2:(nchar(s) - 1)) {
    char <- substr(s, i, i)
    if (char == '[') {
      balance <- balance + 1
    } else if (char == ']') {
      balance <- balance - 1
    } else if (char == ',' && balance == 0) {
      split_index <- i
      break
    }
  }
  left <- parse_snail_number(substr(s, 2, split_index - 1))
  right <- parse_snail_number(substr(s, split_index + 1, nchar(s) - 1))
  return(list(value = NULL, left = left, right = right))
}

is_regular <- function(number) {
  is.null(number$left) && is.null(number$right)
}

add_left <- function(number, value) {
  if (is_regular(number)) {
    number$value <- number$value + value
  } else {
    number$left <- add_left(number$left, value)
  }
  number
}

add_right <- function(number, value) {
  if (is_regular(number)) {
    number$value <- number$value + value
  } else {
    number$right <- add_right(number$right, value)
  }
  number
}

explode <- function(number, depth = 0) {
  if (is_regular(number)) {
    return(list(exploded = FALSE, left_value = 0, right_value = 0, number = number))
  }
  if (depth == 4) {
    left_value <- number$left$value
    right_value <- number$right$value
    return(list(exploded = TRUE, left_value = left_value, right_value = right_value, number = list(value = 0, left = NULL, right = NULL)))
  }
  res_left <- explode(number$left, depth + 1)
  if (res_left$exploded) {
    if (res_left$right_value > 0 && !is.null(number$right)) {
      number$right <- add_left(number$right, res_left$right_value)
    }
    return(list(exploded = TRUE, left_value = res_left$left_value, right_value = 0, number = list(value = number$value, left = res_left$number, right = number$right)))
  }
  res_right <- explode(number$right, depth + 1)
  if (res_right$exploded) {
    if (res_right$left_value > 0 && !is.null(number$left)) {
      number$left <- add_right(number$left, res_right$left_value)
    }
    return(list(exploded = TRUE, left_value = 0, right_value = res_right$right_value, number = list(value = number$value, left = number$left, right = res_right$number)))
  }
  return(list(exploded = FALSE, left_value = 0, right_value = 0, number = number))
}

split <- function(number) {
  if (is_regular(number)) {
    if (number$value >= 10) {
      left_val <- floor(number$value / 2)
      right_val <- ceiling(number$value / 2)
      return(list(split = TRUE, number = list(value = NULL, left = list(value = left_val, left = NULL, right = NULL), right = list(value = right_val, left = NULL, right = NULL))))
    }
    return(list(split = FALSE, number = number))
  }
  res_left <- split(number$left)
  if (res_left$split) {
    return(list(split = TRUE, number = list(value = number$value, left = res_left$number, right = number$right)))
  }
  res_right <- split(number$right)
  if (res_right$split) {
    return(list(split = TRUE, number = list(value = number$value, left = number$left, right = res_right$number)))
  }
  return(list(split = FALSE, number = number))
}

reduce <- function(number) {
  while (TRUE) {
    res_explode <- explode(number)
    if (res_explode$exploded) {
      number <- res_explode$number
      next
    }
    res_split <- split(number)
    if (res_split$split) {
      number <- res_split$number
      next
    }
    break
  }
  number
}

add <- function(number1, number2) {
  new_number <- list(value = NULL, left = number1, right = number2)
  reduce(new_number)
}

magnitude <- function(number) {
  if (is_regular(number)) {
    return(number$value)
  }
  3 * magnitude(number$left) + 2 * magnitude(number$right)
}

deep_copy <- function(number) {
    if (is_regular(number)) {
        return(list(value=number$value, left=NULL, right=NULL))
    }
    return(list(value=number$value, left=deep_copy(number$left), right=deep_copy(number$right)))
}

main <- function() {
  lines <- readLines("input.txt")
  snail_numbers <- lapply(lines, parse_snail_number)
  
  largest_magnitude <- 0
  
  for (i in seq_along(snail_numbers)) {
    for (j in seq_along(snail_numbers)) {
      if (i != j) {
        sum1 <- magnitude(add(deep_copy(snail_numbers[[i]]), deep_copy(snail_numbers[[j]])))
        sum2 <- magnitude(add(deep_copy(snail_numbers[[j]]), deep_copy(snail_numbers[[i]])))
        largest_magnitude <- max(largest_magnitude, sum1, sum2)
      }
    }
  }  
  cat(largest_magnitude, "\n")
}

main()
