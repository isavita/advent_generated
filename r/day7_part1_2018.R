
main <- function() {
    data <- readLines("input.txt")
    steps <- character(0)
    dependencies <- list()

    for (line in data) {
        step <- substr(line, 6, 6)
        next_step <- substr(line, 37, 37)
        steps <- c(steps, step, next_step)
        if (is.null(dependencies[[next_step]])) {
          dependencies[[next_step]] <- character(0)
        }
        dependencies[[next_step]] <- c(dependencies[[next_step]], step)
    }
    
    steps <- unique(steps)
    order <- character(0)

    while (length(steps) > 0) {
      available <- steps[sapply(steps, function(step) {
        is.null(dependencies[[step]]) || all(dependencies[[step]] %in% order)
      })]
      next_step <- min(available)
      order <- c(order, next_step)
      steps <- steps[steps != next_step]
    }

    cat(paste(order, collapse = ""))
}

main()
