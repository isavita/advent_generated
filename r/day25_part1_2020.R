
input <- scan("input.txt")
card_public_key <- input[1]
door_public_key <- input[2]

transform <- function(subject, loop_size) {
  value <- 1
  for (i in 1:loop_size) {
    value <- (value * subject) %% 20201227
  }
  return(value)
}

find_loop_size <- function(public_key) {
  subject <- 7
  value <- 1
  loop_size <- 0
  while (value != public_key) {
    loop_size <- loop_size + 1
    value <- (value * subject) %% 20201227
  }
  return(loop_size)
}

card_loop_size <- find_loop_size(card_public_key)
door_loop_size <- find_loop_size(door_public_key)

encryption_key <- transform(card_public_key, door_loop_size)
print(encryption_key)
