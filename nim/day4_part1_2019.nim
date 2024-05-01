import strutils, sequtils

proc is_valid_password(password: int): bool =
  let password_str = $password
  var has_double = false
  var prev_digit = password_str[0]
  for digit in password_str[1..^1]:
    if digit < prev_digit:
      return false
    if digit == prev_digit:
      has_double = true
    prev_digit = digit
  return has_double

proc count_valid_passwords(range_start, range_end: int): int =
  var count = 0
  for password in range_start..range_end:
    if is_valid_password(password):
      inc count
  return count

let input = readFile("input.txt").strip().split('-')
let range_start = parseInt(input[0])
let range_end = parseInt(input[1])

echo "Number of valid passwords: ", count_valid_passwords(range_start, range_end)