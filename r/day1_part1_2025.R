
input <- scan("input.txt", what = character(), quiet = TRUE)
pos <- 50
cnt <- 0

for (s in input) {
  dir <- substr(s, 1, 1)
  amt <- as.integer(substr(s, 2, nchar(s)))
  pos <- (pos + if (dir == "R") amt else -amt) %% 100
  if (pos == 0) cnt <- cnt + 1
}

cat(cnt, "\n")
