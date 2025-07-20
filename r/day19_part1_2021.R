
main <- function() {
  con <- file("input.txt", "r")
  repeat {
    line <- readLines(con, n = 1)
    if (length(line) == 0) break
    cat(line, "\n", sep = "")
  }
  close(con)
}

main()
