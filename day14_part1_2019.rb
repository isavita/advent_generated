
def parse_input(file)
  reactions = {}
  File.readlines(file).each do |line|
    inputs, output = line.strip.split(" => ")
    amount, chemical = output.split
    reactions[chemical] = {
      amount: amount.to_i,
      inputs: inputs.split(", ").map { |input| 
        quantity, name = input.split
        [name, quantity.to_i]
      }.to_h
    }
  end
  reactions
end

def ore_required_for_fuel(reactions, fuel_amount)
  need = {"FUEL" => fuel_amount}
  excess = Hash.new(0)

  until need.keys == ["ORE"]
    chemical, amount_needed = need.find { |k, v| k != "ORE" }
    amount_needed -= excess[chemical]
    excess[chemical] = 0
    reaction = reactions[chemical]
    times = (amount_needed.to_f / reaction[:amount]).ceil
    excess[chemical] += times * reaction[:amount] - amount_needed
    reaction[:inputs].each do |input_chemical, input_amount|
      need[input_chemical] ||= 0
      need[input_chemical] += input_amount * times
    end
    need.delete(chemical)
  end
  need["ORE"]
end

reactions = parse_input("input.txt")
puts ore_required_for_fuel(reactions, 1)
