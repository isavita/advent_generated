
def read_input(file_name)
  File.read(file_name).scan(/Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)/).map { |sizes, positions| [sizes.to_i, positions.to_i] }
end

def find_time_to_press_button(discs)
  time = 0
  loop do
    if discs.each_with_index.all? { |(size, position), index| (position + time + index + 1) % size == 0 }
      return time
    end
    time += 1
  end
end

discs = read_input("input.txt")
puts find_time_to_press_button(discs)
