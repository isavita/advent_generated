
reactions = {} of String => {name: String, amount: Int32}
ingredients = {} of String => Array(NamedTuple(name: String, amount: Int32))

File.each_line("input.txt") do |line|
  parts = line.chomp.split(" => ")
  output = parts[1].split(" ")
  output_amount = output[0].to_i
  output_name = output[1]
  inputs_list = parts[0].split(", ").map do |input|
    input_parts = input.split(" ")
    {name: input_parts[1], amount: input_parts[0].to_i}
  end
  reactions[output_name] = {name: output_name, amount: output_amount}
  ingredients[output_name] = inputs_list
end

ORE_AVAILABLE = 1_000_000_000_000_i64

def calculate_ore(chem : String, amount : Int64, reactions, ingredients, surplus)
  return amount if chem == "ORE"

  if surplus[chem] >= amount
    surplus[chem] -= amount
    return 0_i64
  end

  amount -= surplus[chem]
  surplus[chem] = 0_i64
  reaction = reactions[chem]
  times = (amount + reaction[:amount] - 1) // reaction[:amount]
  ore = 0_i64

  ingredients[chem].each do |ingredient|
    ore += calculate_ore(ingredient[:name], ingredient[:amount].to_i64 * times, reactions, ingredients, surplus)
  end

  surplus[chem] += times * reaction[:amount] - amount
  ore
end

def max_fuel(reactions, ingredients, ore_available)
  low, high = 0_i64, ore_available
  while low < high
    mid = (low + high + 1) // 2
    surplus = Hash(String, Int64).new(0_i64)
    if calculate_ore("FUEL", mid, reactions, ingredients, surplus) > ore_available
      high = mid - 1
    else
      low = mid
    end
  end
  low
end

puts max_fuel(reactions, ingredients, ORE_AVAILABLE)
