File.open("input.txt", "r") do |file|
  initial_state = file.gets.try(&.strip.split(",").map(&.to_i))
  state = Array.new(9, 0)
  initial_state.try { |is| is.each { |fish| state[fish] += 1 } }
  80.times do
    new_fish = state.shift
    state[6] += new_fish
    state << new_fish
  end
  puts state.sum
end