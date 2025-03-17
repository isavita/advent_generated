
parse_snail_number <- function(s) {
    if (grepl("^\\[.*\\]$", s)) {
        balance <- 0
        split_index <- 0
        s_no_brackets <- substr(s, 2, nchar(s) - 1)
        for (i in 1:nchar(s_no_brackets)) {
            char <- substr(s_no_brackets, i, i)
            if (char == '[') {
                balance <- balance + 1
            } else if (char == ']') {
                balance <- balance - 1
            } else if (char == ',') {
                if (balance == 0) {
                    split_index <- i
                    break
                }
            }
        }
        left <- parse_snail_number(substr(s_no_brackets, 1, split_index - 1))
        right <- parse_snail_number(substr(s_no_brackets, split_index + 1, nchar(s_no_brackets)))
        return(list(left = left, right = right))
    } else {
        return(as.integer(s))
    }
}

add_left <- function(snail_num, value) {
  if (is.list(snail_num)) {
    snail_num$left <- add_left(snail_num$left, value)
    return(snail_num)
  } else {
    return(snail_num + value)
  }
}

add_right <- function(snail_num, value) {
    if (is.list(snail_num)) {
        snail_num$right <- add_right(snail_num$right, value)
        return(snail_num)
    } else {
        return(snail_num + value)
    }
}

explode <- function(snail_num, depth = 0) {
    if (!is.list(snail_num)) {
        return(list(FALSE, 0, 0, snail_num))
    }

    if (depth == 4) {
      
        return(list(TRUE, snail_num$left, snail_num$right, 0))
    }

    exploded_left <- explode(snail_num$left, depth + 1)
    if (exploded_left[[1]]) {
        if (exploded_left[[3]] > 0) {
          snail_num$right <- add_left(snail_num$right, exploded_left[[3]])
        }
        snail_num$left <- exploded_left[[4]]
        return(list(TRUE, exploded_left[[2]], 0, snail_num))
    }

    exploded_right <- explode(snail_num$right, depth + 1)
    if (exploded_right[[1]]) {
        if (exploded_right[[2]] > 0) {
            snail_num$left <- add_right(snail_num$left, exploded_right[[2]])
        }
        snail_num$right <- exploded_right[[4]]
        return(list(TRUE, 0, exploded_right[[3]], snail_num))
    }

    return(list(FALSE, 0, 0, snail_num))
}

split <- function(snail_num) {
    if (!is.list(snail_num)) {
        if (snail_num >= 10) {
            left <- floor(snail_num / 2)
            right <- ceiling(snail_num / 2)
            return(list(TRUE, list(left = left, right = right)))
        }
        return(list(FALSE, snail_num))
    }

    split_left <- split(snail_num$left)
    if (split_left[[1]]) {
        snail_num$left <- split_left[[2]]
        return(list(TRUE, snail_num))
    }

    split_right <- split(snail_num$right)
    if (split_right[[1]]) {
        snail_num$right <- split_right[[2]]
        return(list(TRUE, snail_num))
    }

    return(list(FALSE, snail_num))
}

reduce <- function(snail_num) {
  while (TRUE) {
    exploded_res <- explode(snail_num)
    if (exploded_res[[1]]) {
      snail_num <- exploded_res[[4]]
      next
    }
    split_res <- split(snail_num)
    if (split_res[[1]]) {
      snail_num <- split_res[[2]]
    } else{
      break
    }
  }
  return(snail_num)
}

add <- function(snail_num1, snail_num2) {
  new_number <- list(left = snail_num1, right = snail_num2)
  return(reduce(new_number))
}

magnitude <- function(snail_num) {
    if (!is.list(snail_num)) {
        return(snail_num)
    }
    return(3 * magnitude(snail_num$left) + 2 * magnitude(snail_num$right))
}

main <- function() {
    lines <- readLines("input.txt")
    snail_numbers <- lapply(lines, parse_snail_number)

    result <- snail_numbers[[1]]
    for (i in 2:length(snail_numbers)) {
        result <- add(result, snail_numbers[[i]])
    }
    cat(magnitude(result))
}

main()
