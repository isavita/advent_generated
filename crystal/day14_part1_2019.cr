require "file_utils"

class Chemical
  property name : String
  property amount : Int32

  def initialize(@name, @amount)
  end
end

def parse_chemical(s)
  parts = s.split(" ")
  amount = parts[0].to_i
  Chemical.new(parts[1], amount)
end

def calculate_ore(chem, amount, reactions, ingredients, surplus)
  return amount if chem == "ORE"

  if surplus.has_key?(chem) && surplus[chem] >= amount
    surplus[chem] -= amount
    return 0
  end

  if surplus.has_key?(chem)
    amount -= surplus[chem]
    surplus[chem] = 0
  end

  reaction = reactions[chem]
  times = (amount + reaction.amount - 1) // reaction.amount
  ore = 0

  ingredients[chem].each do |ingredient|
    ore += calculate_ore(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
  end

  surplus[chem] = times * reaction.amount - amount
  ore
end

file = File.open("input.txt")
reactions = {} of String => Chemical
ingredients = {} of String => Array(Chemical)

file.each_line do |line|
  parts = line.split(" => ")
  output = parse_chemical(parts[1])
  inputs = parts[0].split(", ").map { |s| parse_chemical(s) }
  reactions[output.name] = output
  ingredients[output.name] = inputs
end

puts calculate_ore("FUEL", 1, reactions, ingredients, {} of String => Int32)