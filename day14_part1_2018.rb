
input = File.read('input.txt').to_i

recipes = [3, 7]
elf1 = 0
elf2 = 1

while recipes.length < input + 10
  sum = recipes[elf1] + recipes[elf2]
  if sum >= 10
    recipes.push(1)
    recipes.push(sum % 10)
  else
    recipes.push(sum)
  end

  elf1 = (elf1 + recipes[elf1] + 1) % recipes.length
  elf2 = (elf2 + recipes[elf2] + 1) % recipes.length
end

puts recipes[input...input + 10].join
