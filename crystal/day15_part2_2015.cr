
struct Ingredient
  @name : String
  @capacity : Int32
  @durability : Int32
  @flavor : Int32
  @texture : Int32
  @calories : Int32

  def initialize(@name : String, @capacity : Int32, @durability : Int32, @flavor : Int32, @texture : Int32, @calories : Int32)
  end

  def name
    @name
  end

  def capacity
    @capacity
  end

  def durability
    @durability
  end

  def flavor
    @flavor
  end

  def texture
    @texture
  end

  def calories
    @calories
  end
end

def read_ingredients(filename : String) : Array(Ingredient)
  File.open(filename) do |file|
    ingredients = Array(Ingredient).new
    file.each_line do |line|
      parts = line.split
      next if parts.size < 11

      capacity = parts[2][0...-1].to_i
      durability = parts[4][0...-1].to_i
      flavor = parts[6][0...-1].to_i
      texture = parts[8][0...-1].to_i
      calories = parts[10].to_i

      ingredients << Ingredient.new(parts[0], capacity, durability, flavor, texture, calories)
    end
    ingredients
  end
end

def find_max_score(ingredients : Array(Ingredient), total_teaspoons : Int32, target_calories : Int32) : Int32
  calculate_max_score(ingredients, 0, total_teaspoons, Array(Int32).new, target_calories)
end

def calculate_max_score(ingredients : Array(Ingredient), index : Int32, remaining : Int32, teaspoons : Array(Int32), target_calories : Int32) : Int32
  if index == ingredients.size - 1
    teaspoons << remaining
    return score(ingredients, teaspoons) if calculate_calories(ingredients, teaspoons) == target_calories
    return 0
  end

  max_score = 0
  (0..remaining).each do |i|
    score = calculate_max_score(ingredients, index + 1, remaining - i, teaspoons + [i], target_calories)
    max_score = score if score > max_score
  end
  max_score
end

def score(ingredients : Array(Ingredient), teaspoons : Array(Int32)) : Int32
  capacity = durability = flavor = texture = 0
  ingredients.each_with_index do |ingredient, i|
    capacity += ingredient.capacity * teaspoons[i]
    durability += ingredient.durability * teaspoons[i]
    flavor += ingredient.flavor * teaspoons[i]
    texture += ingredient.texture * teaspoons[i]
  end

  capacity = 0 if capacity < 0
  durability = 0 if durability < 0
  flavor = 0 if flavor < 0
  texture = 0 if texture < 0

  capacity * durability * flavor * texture
end

def calculate_calories(ingredients : Array(Ingredient), teaspoons : Array(Int32)) : Int32
  calories = 0
  ingredients.each_with_index do |ingredient, i|
    calories += ingredient.calories * teaspoons[i]
  end
  calories
end

ingredients = read_ingredients("input.txt")
max_score = find_max_score(ingredients, 100, 500)
puts max_score
