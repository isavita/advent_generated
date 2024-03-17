File.open("input.txt", "r") do |file|
  stacks = Array.new(9) { [] of Char }
  file.each_line do |line|
    if line.starts_with?("move")
      move, from, to = line.split(" ")[1], line.split(" ")[3].to_i, line.split(" ")[5].to_i
      move.to_i.times { stacks[to-1].unshift(stacks[from-1].shift) }
    elsif !line.strip.empty?
      9.times { |i| stacks[i] << line[i*4+1] if line[i*4+1] != ' ' }
    end
  end
  puts stacks.map(&.first).join
end