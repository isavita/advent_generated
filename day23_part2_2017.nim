
import math

proc isPrime(n: int): bool =
  for i in 2..int(math.sqrt(float(n))):
    if n mod i == 0:
      return false
  return true

var
  b = 57 * 100 + 100000
  c = b + 17000
  h = 0

for x in countup(b, c, 17):
  if not isPrime(x):
    inc h

echo h
