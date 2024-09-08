class Ingredient
  attr_reader :name, :capacity, :durability, :flavor, :texture, :calories

  def initialize(name, capacity, durability, flavor, texture, calories)
    @name = name
    @capacity = capacity
    @durability = durability
    @flavor = flavor
    @texture = texture
    @calories = calories
  end
end

def parse_ingredients(input)
  input.map do |line|
    name, properties = line.split(': ')
    values = properties.scan(/-?\d+/).map(&:to_i)
    Ingredient.new(name, *values)
  end
end

def calculate_score(ingredients, amounts)
  properties = [:capacity, :durability, :flavor, :texture]
  scores = properties.map do |prop|
    [ingredients.zip(amounts).sum { |ing, amt| ing.send(prop) * amt }, 0].max
  end
  scores.inject(:*)
end

def find_best_recipe(ingredients, total_amount, current_amounts = [], best_score = 0)
  if ingredients.size == current_amounts.size + 1
    last_amount = total_amount - current_amounts.sum
    return best_score if last_amount < 0

    amounts = current_amounts + [last_amount]
    score = calculate_score(ingredients, amounts)
    return [score, best_score].max
  end

  (0..total_amount).each do |amount|
    remaining = total_amount - current_amounts.sum - amount
    break if remaining < 0
    score = find_best_recipe(ingredients, total_amount, current_amounts + [amount], best_score)
    best_score = score if score > best_score
  end

  best_score
end

# Example usage
input = [
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
  "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
]

ingredients = parse_ingredients(input)
best_score = find_best_recipe(ingredients, 100)
puts best_score
