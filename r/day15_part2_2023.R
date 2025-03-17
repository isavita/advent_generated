
hash_string <- function(s) {
  res <- 0
  for (char in strsplit(s, "")[[1]]) {
    res <- (res + utf8ToInt(char)) * 17
    res <- res %% 256
  }
  return(res)
}

parse_step <- function(step_str) {
  label <- gsub("[-=0-9]", "", step_str)
  num_box <- hash_string(label)
  operation <- substr(step_str, nchar(label) + 1, nchar(label) + 1)
  number <- ifelse(operation == "=", as.integer(substr(step_str, nchar(label) + 2, nchar(step_str))), NA)
  list(label = label, num_box = num_box, operation = operation, number = number)
}

get_boxes <- function(steps_str) {
    boxes <- list()
    for (step_str in steps_str) {
        step <- parse_step(step_str)
        box_contents <- boxes[[as.character(step$num_box)]]

        if (is.null(box_contents)) {
          box_contents <- list()
        }
        
        if (step$operation == "-") {
            new_box_contents <- list()
            for (content in box_contents) {
                if (names(content) != step$label) {
                    new_box_contents[[length(new_box_contents) + 1]] <- content
                }
            }
            box_contents <- new_box_contents
        } else if (step$operation == "=") {
          found <- FALSE
            for (i in seq_along(box_contents)) {
               if (names(box_contents[[i]]) == step$label) {
                  
                  box_contents[[i]][[1]] <- step$number
                  found <- TRUE
                  break
               }
            }
            if(!found){
              box_contents[[length(box_contents) + 1]] <- setNames(list(step$number), step$label)
            }
        }

        if (length(box_contents) == 0) {
          boxes[[as.character(step$num_box)]] <- NULL
        } else{
          boxes[[as.character(step$num_box)]] <- box_contents
        }
    }
    return(boxes)
}

calculate_power <- function(boxes) {
  res <- 0
  for (i_box in 0:255) {
    box_name <- as.character(i_box)
    if (!is.null(boxes[[box_name]])) {
      for (i_slot in seq_along(boxes[[box_name]])) {
        content <- boxes[[box_name]][[i_slot]]
        for (value in content) {
            res <- res + (i_box + 1) * i_slot * value
        }
      }
    }
  }
  return(res)
}

solve <- function(input) {
  line <- input[1]
  steps_str <- strsplit(line, ",")[[1]]
  boxes <- get_boxes(steps_str)
  return(calculate_power(boxes))
}

main <- function() {
  input <- readLines("input.txt")
  cat(solve(input))
}

main()
