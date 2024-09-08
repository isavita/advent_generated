class Reaction
  attr_reader :inputs, :output, :quantity

  def initialize(inputs, output, quantity)
    @inputs = inputs
    @output = output
    @quantity = quantity
  end
end

def parse_reactions(input)
  reactions = {}
  input.each_line do |line|
    inputs, output = line.strip.split(' => ')
    output_quantity, output_chemical = output.split
    inputs = inputs.split(', ').map { |input| input.split.map(&:to_i).zip(input.split.map(&:to_s)).first }
    reactions[output_chemical] = Reaction.new(inputs, output_chemical, output_quantity.to_i)
  end
  reactions
end

def ore_required_for_fuel(reactions, fuel_amount)
  required = { 'FUEL' => fuel_amount }
  ore_count = 0

  until required.keys == ['ORE']
    chemical, amount = required.find { |k, _| k != 'ORE' }
    reaction = reactions[chemical]
    multiplier = (amount.to_f / reaction.quantity).ceil
    required.delete(chemical)

    reaction.inputs.each do |input_amount, input_chemical|
      if input_chemical == 'ORE'
        ore_count += input_amount * multiplier
      else
        required[input_chemical] ||= 0
        required[input_chemical] += input_amount * multiplier
      end
    end
  end

  ore_count
end

def max_fuel_for_ore(reactions, total_ore)
  low = 0
  high = total_ore

  while low < high
    mid = (low + high + 1) / 2
    ore_needed = ore_required_for_fuel(reactions, mid)

    if ore_needed <= total_ore
      low = mid
    else
      high = mid - 1
    end
  end

  low
end

input = File.read('input.txt')
reactions = parse_reactions(input)

# Part 1
puts "Part 1: #{ore_required_for_fuel(reactions, 1)}"

# Part 2
total_ore = 1_000_000_000_000
puts "Part 2: #{max_fuel_for_ore(reactions, total_ore)}"
