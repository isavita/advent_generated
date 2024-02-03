
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

allergen_map.each do |allergen, ingredients|
  ingredients.each { |ingredient| allergen_ingredients[ingredient] = true }
end

safe_ingredients = ingredient_count.select { |ingredient, count| !allergen_ingredients[ingredient] }
puts safe_ingredients.values.sum
