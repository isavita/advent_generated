import strutils, sequtils

# Custom function to check if a string is an integer
proc isInteger(s: string): bool =
  try:
    discard parseInt(s)
    true
  except ValueError:
    false

let expenses = readFile("input.txt").split("\n")
  .filter(isInteger)  # Use the custom function to filter out non-integer strings
  .map(parseInt)
  .toSeq()

for i in 0 ..< expenses.len():
  for j in i+1 ..< expenses.len():
    if expenses[i] + expenses[j] == 2020:
      echo expenses[i] * expenses[j]
      quit()
