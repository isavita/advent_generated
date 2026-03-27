
data <- readLines("input.txt", warn = FALSE)
data <- data[data != ""]

cur <- 50
total <- 0
dial_size <- 100

for (line in data) {
  dirc <- substr(line, 1, 1)
  amt <- as.numeric(substring(line, 2))
  
  if (dirc == "R") {
    total <- total + (cur + amt) %/% dial_size
    cur <- (cur + amt) %% dial_size
  } else {
    total <- total + (cur - 1) %/% dial_size - (cur - amt - 1) %/% dial_size
    cur <- (cur - amt) %% dial_size
  }
}

cat(paste0("The password is: ", total, "\n"))
