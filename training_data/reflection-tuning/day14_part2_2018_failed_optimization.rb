def solve_part_two(target)
  recipes = "37"
  elf1, elf2 = 0, 1
  target_length = target.length

  loop do
    sum = recipes[elf1].to_i + recipes[elf2].to_i
    new_recipes = sum.to_s
    recipes << new_recipes

    elf1 = (elf1 + 1 + recipes[elf1].to_i) % recipes.length
    elf2 = (elf2 + 1 + recipes[elf2].to_i) % recipes.length

    # Only check for the target when we've added enough new recipes
    if recipes.length >= target_length
      # Check the last target_length + 1 characters (in case the target spans the new recipes)
      check_string = recipes[-target_length - 1..-1]
      index = check_string.index(target)
      if index
        return recipes.length - target_length - 1 + index
      end
    end
  end
end

# Example usage
puts solve_part_two("51589")  # Should output 9
puts solve_part_two("01245")  # Should output 5
puts solve_part_two("92510")  # Should output 18
puts solve_part_two("59414")  # Should output 2018

# Solve for the actual puzzle input
puzzle_input = "1413131339"
puts solve_part_two(puzzle_input)
