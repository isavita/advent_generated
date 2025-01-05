
input <- scan("input.txt", what = character(), sep = ",")
starting_numbers <- as.integer(input)
last_spoken <- integer(10000000)
last_number <- 0
next_number <- 0

for (turn in 1:2020) {
  if (turn <= length(starting_numbers)) {
    last_number <- starting_numbers[turn]
    last_spoken[last_number+1] <- turn
    next
  }
  if (last_spoken[last_number+1] != 0 && last_spoken[last_number+1] != turn - 1) {
    next_number <- turn - 1 - last_spoken[last_number+1]
  } else {
    next_number <- 0
  }
  last_spoken[last_number+1] <- turn - 1
  last_number <- next_number
}

cat(last_number, "\n")
