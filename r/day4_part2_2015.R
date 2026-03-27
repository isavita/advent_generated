
if (!require("digest", quietly = TRUE)) install.packages("digest", repos = "https://cloud.r-project.org")

input_key <- trimws(readLines("input.txt", warn = FALSE)[1])
i <- 0

while (TRUE) {
  hash_val <- digest::digest(paste0(input_key, i), algo = "md5", serialize = FALSE)
  if (startsWith(hash_val, "000000")) {
    cat(i, "\n")
    break
  }
  i <- i + 1
}
