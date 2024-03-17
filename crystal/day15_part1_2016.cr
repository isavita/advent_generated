require "file"

class Disc
  property total_positions : Int32
  property start_position : Int32

  def initialize(@total_positions, @start_position)
  end
end

discs = [] of Disc
File.each_line("input.txt") do |line|
  matches = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\./.match(line)
  if matches
    discs << Disc.new(matches[2].to_i, matches[3].to_i)
  end
end

time = 0
loop do
  if check_discs(discs, time)
    puts time
    break
  end
  time += 1
end

def check_discs(discs, time)
  discs.each_with_index do |disc, i|
    position = (disc.start_position + time + i + 1) % disc.total_positions
    return false if position != 0
  end
  true
end