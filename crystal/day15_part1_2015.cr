struct Ingredient
  getter name : String
  getter capacity : Int32
  getter durability : Int32
  getter flavor : Int32
  getter texture : Int32

  def initialize(@name : String, @capacity : Int32, @durability : Int32, @flavor : Int32, @texture : Int32)
  end
end

def read_ingredients(filename : String) : Array(Ingredient)
  ingredients = [] of Ingredient
  File.open(filename) do |file|
    file.each_line do |line|
      parts = line.split
      next if parts.size < 11

      capacity = parts[2][0...-1].to_i
      durability = parts[4][0...-1].to_i
      flavor = parts[6][0...-1].to_i
      texture = parts[8][0...-1].to_i

      ingredients << Ingredient.new(parts[0], capacity, durability, flavor, texture)
    end
  end
  ingredients
end

def find_max_score(ingredients : Array(Ingredient), total_teaspoons : Int32) : Int32
  calculate_max_score(ingredients, 0, total_teaspoons, [] of Int32)
end

def calculate_max_score(ingredients : Array(Ingredient), index : Int32, remaining : Int32, teaspoons : Array(Int32)) : Int32
  if index == ingredients.size - 1
    teaspoons << remaining
    score(ingredients, teaspoons)
  else
    max_score = 0
    (0..remaining).each do |i|
      score = calculate_max_score(ingredients, index + 1, remaining - i, teaspoons + [i])
      max_score = score if score > max_score
    end
    max_score
  end
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

ingredients = read_ingredients("input.txt")
max_score = find_max_score(ingredients, 100)
puts max_score