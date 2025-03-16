
parse_input <- function(file_path) {
  lines <- readLines(file_path)
  blank_line <- which(lines == "")
  
  workflows_str <- lines[1:(blank_line - 1)]
  parts_str <- lines[(blank_line + 1):length(lines)]
  
  workflows <- list()
  for (workflow_str in workflows_str) {
    name <- sub("\\{.*", "", workflow_str)
    rules_str <- sub(".*\\{(.*)\\}", "\\1", workflow_str)
    rules_vec <- strsplit(rules_str, ",")[[1]]
    
    rules <- lapply(rules_vec, function(rule_str) {
      if (grepl(":", rule_str)) {
        condition <- sub(":.*", "", rule_str)
        destination <- sub(".*:", "", rule_str)
        list(condition = condition, destination = destination)
      } else {
        list(condition = NULL, destination = rule_str)
      }
    })
    
    workflows[[name]] <- rules
  }
  
  parts <- lapply(parts_str, function(part_str) {
    values <- as.integer(gsub("[^0-9,]", "", unlist(strsplit(part_str, ","))))
    names(values) <- c("x", "m", "a", "s")
    as.list(values) # Convert to a named list.  Crucially important.
  })
  
  list(workflows = workflows, parts = parts)
}

evaluate_condition <- function(condition, part) {
  if (is.null(condition)) {
    return(TRUE)
  }
    var <- substr(condition, 1, 1)
    op <- substr(condition, 2, 2)
    val <- as.integer(substr(condition, 3, nchar(condition)))
    
    if (op == ">") {
        return(part[[var]] > val)
    } else if (op == "<") {
        return(part[[var]] < val)
    }
    return(FALSE) # Should not get here, but safe return
}

process_part <- function(part, workflows) {
    current_workflow <- "in"

    while (current_workflow != "A" && current_workflow != "R") {
        rules <- workflows[[current_workflow]]
        for (rule in rules) {
            if (evaluate_condition(rule$condition, part)) {
                current_workflow <- rule$destination
                break
            }
        }
    }
  return(current_workflow)
}

main <- function() {
  parsed_data <- parse_input("input.txt")
  workflows <- parsed_data$workflows
  parts <- parsed_data$parts
  
  total_rating <- 0
  for (part in parts) {
    result <- process_part(part, workflows)
    if (result == "A") {
      total_rating <- total_rating + sum(unlist(part))
    }
  }
  
  cat(total_rating, "\n")
}

main()
