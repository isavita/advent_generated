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

def parse_ingredients(file_path)
  File.readlines(file_path).map do |line|
    name, props = line.strip.split(': ')
    values = props.split(', ').map { |prop| prop.split(' ')[1].to_i }
    Ingredient.new(name, *values)
  end
end

def calculate_score(combination, ingredients)
  properties = [0, 0, 0, 0, 0]
  combination.each_with_index do |amount, i|
    properties[0] += amount * ingredients[i].capacity
    properties[1] += amount * ingredients[i].durability
    properties[2] += amount * ingredients[i].flavor
    properties[3] += amount * ingredients[i].texture
    properties[4] += amount * ingredients[i].calories
  end
  properties.map! { |p| [p, 0].max }
  [properties[0..3].inject(:*), properties[4]]
end

def find_best_combination(ingredients, target_sum, index = 0, current = [], best_score = 0, calorie_limit = nil)
  if index == ingredients.size - 1
    current << target_sum
    score, calories = calculate_score(current, ingredients)
    return [score, current] if calorie_limit.nil? || calories == calorie_limit
    return [0, []]
  end

  (0..target_sum).each do |amount|
    new_current = current + [amount]
    new_target = target_sum - amount
    score, combination = find_best_combination(ingredients, new_target, index + 1, new_current, best_score, calorie_limit)
    best_score = [best_score, score].max
  end

  [best_score, []]
end

ingredients = parse_ingredients('input.txt')

part1_score, _ = find_best_combination(ingredients, 100)
puts "Part 1: #{part1_score}"

part2_score, _ = find_best_combination(ingredients, 100, 0, [], 0, 500)
puts "Part 2: #{part2_score}"
