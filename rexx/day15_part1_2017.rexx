
/* Rexx */
numeric digits 20

file = 'input.txt'
genA = linein(file)
genB = linein(file)
call linein file, 1, 0

genAFactor = 16807
genBFactor = 48271
modulus = 2147483647
matches = 0

do 40000000
  genA = (genA * genAFactor) // modulus
  genB = (genB * genBFactor) // modulus
  if (genA // 65536) = (genB // 65536) then
    matches = matches + 1
end

say matches
