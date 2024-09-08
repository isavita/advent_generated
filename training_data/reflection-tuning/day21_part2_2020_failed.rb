require 'set'

# Parse input
foods = File.readlines('input.txt', chomp: true).map do |line|
  ingredients, allergens = line.split(' (contains ')
  [ingredients.split(' '), allergens&.chomp(')')&.split(', ') || []]
end

# Create a hash to store possible ingredients for each allergen
allergen_possibilities = Hash.new { |h, k| h[k] = nil }

foods.each do |ingredients, allergens|
  allergens.each do |allergen|
    if allergen_possibilities[allergen].nil?
      allergen_possibilities[allergen] = Set.new(ingredients)
    else
      allergen_possibilities[allergen] &= ingredients
    end
  end
end

# Find ingredients that can't be allergens
all_ingredients = foods.flat_map(&:first).uniq
safe_ingredients = all_ingredients - allergen_possibilities.values.reduce(Set.new, :|)

# Count occurrences of safe ingredients
safe_count = foods.sum { |ingredients, _| (ingredients & safe_ingredients.to_a).size }
puts "Part 1: #{safe_count}"

# Determine which ingredient contains which allergen
allergen_map = {}
while allergen_possibilities.any? { |_, v| v.size > 1 }
  solved = allergen_possibilities.find { |_, v| v.size == 1 }
  ingredient = solved[1].first
  allergen_map[solved[0]] = ingredient
  allergen_possibilities.each { |_, v| v.delete(ingredient) }
end

# Add any remaining single-ingredient allergens
allergen_possibilities.each do |allergen, ingredients|
  allergen_map[allergen] = ingredients.first if ingredients.size == 1
end

# Create the canonical dangerous ingredient list
canonical_list = allergen_map.sort_by(&:first).map(&:last).join(',')
puts "Part 2: #{canonical_list}"
