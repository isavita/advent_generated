masses = File.readlines('input.txt').map(&:to_i)

def calculate_fuel(mass)
  fuel = (mass / 3) - 2
  fuel <= 0 ? 0 : fuel + calculate_fuel(fuel)
end

total_fuel = masses.sum { |mass| calculate_fuel(mass) }

puts total_fuel