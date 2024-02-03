
input = File.read('input.txt').split("\n")
count = 0
input.each do |line|
  output_values = line.split(" | ")[1].split
  output_values.each do |value|
    count += 1 if [2, 4, 3, 7].include?(value.length)
  end
end
puts count
