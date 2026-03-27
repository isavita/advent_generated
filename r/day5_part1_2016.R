
if (!requireNamespace("openssl", quietly = TRUE)) install.packages("openssl", repos = "https://cran.rstudio.com")

id <- trimws(readLines("input.txt", warn = FALSE)[1])
password <- ""
i <- 0

while (nchar(password) < 8) {
  h <- as.character(openssl::md5(paste0(id, i)))
  if (startsWith(h, "00000")) {
    password <- paste0(password, substr(h, 6, 6))
  }
  i <- i + 1
}

cat(password, "\n")
