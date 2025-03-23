
def min_max_keys(m : Hash(Int32, Char)) : Tuple(Int32, Int32)
  min_key = m.keys.min
  max_key = m.keys.max
  {min_key, max_key}
end

def state_pattern(m : Hash(Int32, Char)) : Tuple(String, Int64)
  min_pot, max_pot = min_max_keys(m)
  pattern = (min_pot..max_pot).map { |i| m.fetch(i, '.') }.join
  sum_ = m.select { |_, v| v == '#' }.keys.sum.to_i64
  {pattern, sum_}
end

def main
  lines = File.read_lines("input.txt")

  initial_state = ""
  rules = Hash(String, Char).new

  lines.each do |line|
    if line.includes?("initial state")
      initial_state = line.split(": ")[1].strip
    elsif line.includes?("=>")
      parts = line.split(" => ")
      rules[parts[0].strip] = parts[1].strip[0]
    end
  end

  state = Hash(Int32, Char).new
  initial_state.each_char.with_index do |c, i|
    state[i.to_i32] = '#' if c == '#'
  end

  previous_pattern = ""
  previous_sum : Int64 = 0
  offset : Int64 = 0

  50_000_000_000.times do |generation|
    new_state = Hash(Int32, Char).new
    min_pot, max_pot = min_max_keys(state)

    (min_pot - 2).upto(max_pot + 2) do |i|
      pattern = (-2..2).map { |j| state.fetch(i + j, '.') }.join
      if rules.has_key?(pattern) && rules[pattern] == '#'
        new_state[i] = '#'
      end
    end

    state = new_state

    current_pattern, current_sum = state_pattern(state)
    if current_pattern == previous_pattern
      offset = current_sum - previous_sum
      remaining_generations = 50_000_000_000 - generation - 1
      final_sum = current_sum + offset * remaining_generations
      puts final_sum
      return
    end
    previous_pattern = current_pattern
    previous_sum = current_sum
  end
end

main
