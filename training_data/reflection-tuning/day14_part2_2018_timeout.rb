def solve_chocolate_charts(input)
  recipes = [3, 7]
  elf1, elf2 = 0, 1
  input_num = input.to_i
  input_digits = input.chars.map(&:to_i)

  # Part 1
  while recipes.size < input_num + 10
    sum = recipes[elf1] + recipes[elf2]
    recipes.push(sum / 10) if sum >= 10
    recipes.push(sum % 10)
    elf1 = (elf1 + 1 + recipes[elf1]) % recipes.size
    elf2 = (elf2 + 1 + recipes[elf2]) % recipes.size
  end
  part1 = recipes[input_num, 10].join

  # Part 2
  while true
    sum = recipes[elf1] + recipes[elf2]
    new_recipes = sum >= 10 ? [sum / 10, sum % 10] : [sum % 10]
    new_recipes.each do |recipe|
      recipes << recipe
      if recipes[-input_digits.size..-1] == input_digits
        puts "Part 1: #{part1}"
        puts "Part 2: #{recipes.size - input_digits.size}"
        return
      end
    end
    elf1 = (elf1 + 1 + recipes[elf1]) % recipes.size
    elf2 = (elf2 + 1 + recipes[elf2]) % recipes.size
  end
end

input = File.read('input.txt').strip
solve_chocolate_charts(input)
