File.open("input.txt", "r") do |file|
  total_fuel = 0
  file.each_line do |line|
    mass = line.to_i
    fuel = (mass / 3).floor - 2
    total_fuel += fuel

    while fuel > 0
      fuel = (fuel / 3).floor - 2
      total_fuel += fuel > 0 ? fuel : 0
    end
  end
  puts total_fuel
end