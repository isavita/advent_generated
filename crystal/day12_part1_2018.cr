require "file_utils"

file = File.open("input.txt")
initial_state = ""
rules = {} of String => Char

file.each_line do |line|
  if line.includes?("initial state")
    initial_state = line.split(": ")[1]
  elsif line.includes?("=>")
    parts = line.split(" => ")
    rules[parts[0]] = parts[1][0]
  end
end

state = {} of Int32 => Char
initial_state.each_char_with_index do |c, i|
  if c == '#'
    state[i] = '#'
  end
end

20.times do
  new_state = {} of Int32 => Char
  min_pot, max_pot = state.keys.minmax
  (min_pot - 2..max_pot + 2).each do |i|
    pattern = ""
    (i - 2..i + 2).each do |j|
      if state[j]?
        pattern += "#"
      else
        pattern += "."
      end
    end
    if rules[pattern] == '#'
      new_state[i] = '#'
    end
  end
  state = new_state
end

sum = state.keys.sum
puts sum