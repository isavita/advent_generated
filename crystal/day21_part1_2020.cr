
def parse_foods(input)
  input.lines.map do |line|
    ingredients, allergens = line.split(" (contains ")
    {
      ingredients: ingredients.split,
      allergens: allergens.chomp(")").split(", ")
    }
  end
end

def find_safe_ingredients(foods)
  all_ingredients = foods.flat_map { |food| food[:ingredients] }
  all_allergens = foods.flat_map { |food| food[:allergens] }.uniq

  possible_allergen_ingredients = {} of String => Set(String)
  all_allergens.each do |allergen|
    foods_with_allergen = foods.select { |food| food[:allergens].includes?(allergen) }
    possible_ingredients = foods_with_allergen
      .map { |food| food[:ingredients].to_set }
      .reduce { |acc, ingredients| acc & ingredients }
    possible_allergen_ingredients[allergen] = possible_ingredients
  end

  safe_ingredients = all_ingredients.to_set - possible_allergen_ingredients.values.reduce(Set(String).new) { |acc, ingredients| acc | ingredients }
  safe_ingredients.map { |ingredient| all_ingredients.count(ingredient) }.sum
end

# Read input from file
input = File.read("input.txt")
foods = parse_foods(input)
result = find_safe_ingredients(foods)
puts result
