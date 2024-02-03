
total_fuel = 0
File.open("input.txt", "r") do |file|
  file.each_line do |line|
    mass = line.to_i
    fuel = (mass / 3).floor - 2
    total_fuel += fuel
  end
end

puts total_fuel
