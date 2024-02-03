
reactions = {}
ingredients = {}

File.open("input.txt").each do |line|
    parts = line.chomp.split(" => ")
    output = parts[1].split(" ")
    output_amount = output[0].to_i
    output_name = output[1]
    inputs = parts[0].split(", ")
    inputs_list = []
    inputs.each do |input|
        input_parts = input.split(" ")
        input_amount = input_parts[0].to_i
        input_name = input_parts[1]
        inputs_list << {name: input_name, amount: input_amount}
    end
    reactions[output_name] = {name: output_name, amount: output_amount}
    ingredients[output_name] = inputs_list
end

ore_available = 1000000000000

def calculate_ore(chem, amount, reactions, ingredients, surplus)
    if chem == "ORE"
        return amount
    end

    if surplus[chem] >= amount
        surplus[chem] -= amount
        return 0
    end

    amount -= surplus[chem]
    surplus[chem] = 0
    reaction = reactions[chem]
    times = (amount + reaction[:amount] - 1) / reaction[:amount]
    ore = 0

    ingredients[chem].each do |ingredient|
        ore += calculate_ore(ingredient[:name], ingredient[:amount]*times, reactions, ingredients, surplus)
    end

    surplus[chem] += times*reaction[:amount] - amount
    return ore
end

def max_fuel(reactions, ingredients, ore_available)
    low, high = 0, ore_available
    while low < high
        mid = (low + high + 1) / 2
        if calculate_ore("FUEL", mid, reactions, ingredients, Hash.new(0)) > ore_available
            high = mid - 1
        else
            low = mid
        end
    end
    return low
end

puts max_fuel(reactions, ingredients, ore_available)
