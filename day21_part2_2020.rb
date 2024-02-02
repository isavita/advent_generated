
input = File.readlines('input.txt').map(&:chomp)

allergen_map = {}
ingredient_count = Hash.new(0)
allergen_ingredients = {}

input.each do |line|
  ingredients, allergens = line.split(' (contains ')
  ingredients = ingredients.split(' ')
  allergens = allergens[0..-2].split(', ')

  ingredients.each { |ingredient| ingredient_count[ingredient] += 1 }

  allergens.each do |allergen|
    if allergen_map[allergen]
      allergen_map[allergen] &= ingredients
    else
      allergen_map[allergen] = ingredients
    end
  end
end

loop do
  allergen_map.each do |allergen, ingredients|
    if ingredients.length == 1
      allergen_ingredients[allergen] = ingredients.first
      allergen_map.each { |a, i| allergen_map[a] -= ingredients if a != allergen }
    end
  end

  break if allergen_map.all? { |_, ingredients| ingredients.length == 1 }
end

safe_ingredients = ingredient_count.keys - allergen_ingredients.values
puts safe_ingredients.sum { |ingredient| ingredient_count[ingredient] }

puts allergen_ingredients.sort_by { |allergen, _| allergen }.map { |_, ingredient| ingredient }.join(',')
