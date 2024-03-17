File.open("input.txt", "r") do |file|
  recipes = [3, 7]
  elf1 = 0
  elf2 = 1
  input = file.gets.try(&.to_i) || 0 # Use try(&.to_i) to handle the case where file.gets returns nil
  until recipes.size >= input + 10
    sum = recipes[elf1] + recipes[elf2]
    sum.to_s.each_char { |c| recipes << c.to_i }
    elf1 = (elf1 + 1 + recipes[elf1]) % recipes.size
    elf2 = (elf2 + 1 + recipes[elf2]) % recipes.size
  end
  puts recipes[input, 10].join
end