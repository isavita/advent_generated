File.open("input.txt", "r") do |file|
  input, steps = file.read.split("\n\n")
  stacks = Array.new((input.lines.first.length + 1) / 4) { [] }
  input.lines.reverse_each do |line|
    line.chars.each_slice(4).with_index do |chunk, i|
      stacks[i] << chunk[1] if chunk[1] >= 'A' && chunk[1] <= 'Z'
    end
  end

  steps.lines.each do |step|
    n, from, to = step.scan(/\d+/).map(&:to_i)
    n.times { stacks[to - 1] << stacks[from - 1].pop }
  end

  puts stacks.map(&:last).join
end