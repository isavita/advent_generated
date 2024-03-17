File.open("input.txt") do |file|
  instructions = file.gets_to_end.split("\n").map(&.split)
  acc = 0
  i = 0
  visited = Set(Int32).new

  while !visited.includes?(i)
    visited.add(i)
    op, arg = instructions[i]
    case op
    when "acc"
      acc += arg.to_i
      i += 1
    when "jmp"
      i += arg.to_i
    when "nop"
      i += 1
    end
  end

  puts acc
end