memory_game <- function(starting_numbers, n) {
  last_spoken <- integer(n)
  for (i in seq_along(starting_numbers)) {
    last_spoken[starting_numbers[i] + 1] <- i
  }
  
  last_number <- tail(starting_numbers, 1)
  turn <- length(starting_numbers) + 1
  
  while (turn <= n) {
    if (last_spoken[last_number + 1] == 0) {
      new_number <- 0
    } else {
      new_number <- turn - 1 - last_spoken[last_number + 1]
    }
    last_spoken[last_number + 1] <- turn - 1
    last_number <- new_number
    turn <- turn + 1
  }
  
  return(last_number)
}

main <- function() {
  input <- scan("input.txt", what = integer(), sep = ",")
  
  result_2020 <- memory_game(input, 2020)
  cat("2020th number spoken:", result_2020, "\n")
  
  result_30000000 <- memory_game(input, 30000000)
  cat("30000000th number spoken:", result_30000000, "\n")
}

main()