
initial_state = ""
rules = {}

File.open("input.txt").each_with_index do |line, index|
  if index == 0
    initial_state = line.split(": ")[1].strip
  elsif index > 1
    rule = line.split(" => ")
    rules[rule[0]] = rule[1].strip
  end
end

generations = 20
state = initial_state
offset = 0

generations.times do
  state = "...." + state + "...."
  new_state = ""

  (2..state.length - 3).each do |i|
    pattern = state[i - 2..i + 2]
    new_state += rules[pattern] || "."
  end

  offset += 2
  state = new_state
end

sum = 0
state.chars.each_with_index do |pot, index|
  sum += index - offset if pot == "#"
end

puts sum
