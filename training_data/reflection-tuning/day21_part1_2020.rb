require 'set'

# Read and parse input
foods = File.readlines('input.txt', chomp: true).map do |line|
  ingredients, allergens = line.split(' (contains ')
  [
    Set.new(ingredients.split),
    allergens ? Set.new(allergens.chomp(')').split(', ')) : Set.new
  ]
end

# Create allergen to possible ingredients mapping
allergen_map = {}
foods.each do |ingredients, allergens|
  allergens.each do |allergen|
    if allergen_map[allergen]
      allergen_map[allergen] &= ingredients
    else
      allergen_map[allergen] = ingredients.dup
    end
  end
end

# Find all ingredients and potentially allergenic ingredients
all_ingredients = foods.map(&:first).reduce(:|)
potentially_allergenic = allergen_map.values.reduce(:|)

# Find safe ingredients
safe_ingredients = all_ingredients - potentially_allergenic

# Count occurrences of safe ingredients
count = foods.sum { |ingredients, _| (ingredients & safe_ingredients).size }

puts count
