masses = [] of Int32
total = 0.0

def process_line(line : String, masses : Array(Int32))
  m = line.to_i
  masses << m
end

def get_total(masses : Array(Int32) )
  temp_total = 0.0

  masses.each do |mass|
    temp_total += (mass / 3).floor - 2
  end

  return temp_total
end

file = File.open("input.txt")
file.each_line do |line|
  process_line(line, masses)
end
file.close

total = get_total(masses)

puts total