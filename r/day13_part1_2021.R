# Read input data
input <- readLines("input.txt")

# Split input into dots and folds
dots <- do.call(rbind, strsplit(input[grepl(",", input)], ","))
folds <- input[grepl("fold along", input)]

# Convert dots to numeric
dots <- as.data.frame(dots)
colnames(dots) <- c("x", "y")
dots$x <- as.numeric(as.character(dots$x))
dots$y <- as.numeric(as.character(dots$y))

# Process the first fold instruction
first_fold <- sub("fold along ", "", folds[1])
axis_line <- strsplit(first_fold, "=")[[1]]
axis <- axis_line[1]
line <- as.numeric(axis_line[2])

# Apply the fold
if (axis == "y") {
  dots$y <- ifelse(dots$y > line, line - (dots$y - line), dots$y)
} else {
  dots$x <- ifelse(dots$x > line, line - (dots$x - line), dots$x)
}

# Count unique visible dots
visible_dots <- nrow(unique(dots))

# Print the result
cat(visible_dots, "\n")