File.open("input.txt") do |file|
  stacks = [] of Array(Char)
  instructions = [] of Tuple(Int32, Int32, Int32)

  file.each_line do |line|
    if line.starts_with?("move")
      parts = line.split
      instructions << {parts[1].to_i, parts[3].to_i, parts[5].to_i}
    elsif line.strip.size > 0
      stack_index = 0
      line.chars.each_slice(4) do |slice|
        if stacks.size <= stack_index
          stacks << [] of Char
        end
        if slice[1] != ' '
          stacks[stack_index] << slice[1]
        end
        stack_index += 1
      end
    end
  end

  stacks.each(&.reverse!)

  instructions.each do |count, from, to|
    crates = stacks[from - 1].pop(count)
    stacks[to - 1].concat(crates)
  end

  puts stacks.map(&.last).join
end