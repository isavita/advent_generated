
if (!require("jsonlite", quietly = TRUE)) install.packages("jsonlite", repos='http://cran.us.r-project.org')
library(jsonlite)

raw_text <- paste(readLines("input.txt", warn = FALSE), collapse = "")

nums <- as.numeric(unlist(regmatches(raw_text, gregexpr("-?\\d+", raw_text))))
p1_sum <- sum(nums, na.rm = TRUE)

walk <- function(x) {
  if (is.numeric(x)) {
    return(sum(x))
  }
  if (!is.list(x)) {
    return(0)
  }
  if (!is.null(names(x))) {
    has_red <- any(vapply(x, function(v) identical(v, "red"), logical(1)))
    if (has_red) {
      return(0)
    }
  }
  sum(vapply(x, walk, numeric(1)))
}

data <- fromJSON(raw_text, simplifyVector = FALSE)
p2_sum <- walk(data)

cat(sprintf("Part 1 Sum: %s\n", format(p1_sum, scientific = FALSE)))
cat(sprintf("Part 2 Sum: %s\n", format(p2_sum, scientific = FALSE)))
