count <- 0
con <- file("input.txt", "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  output <- strsplit(line, " \\| ")[[1]][2]
  digits <- strsplit(output, " ")[[1]]
  count <- count + sum(nchar(digits) %in% c(2, 4, 3, 7))
}
close(con)
print(count)