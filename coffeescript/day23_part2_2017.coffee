fs = require 'fs'

isPrime = (n) ->
  return false if n <= 1
  for i in [2..Math.sqrt(n)]
    return false if n % i == 0
  true

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  b = 57 * 100 + 100000
  c = b + 17000
  h = 0

  for x in [b..c] by 17
    h++ unless isPrime(x)

  console.log h