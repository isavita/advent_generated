
def read_input(filename : String) : Int32
  File.open(filename) do |file|
    total_elves_str = file.gets
    if total_elves_str.nil?
      raise "File is empty"
    end
    total_elves = total_elves_str.to_i
    return total_elves
  end
end

def find_winning_elf(total_elves : Int32) : Int32
  highest_power_of_two = 1
  while highest_power_of_two * 2 <= total_elves
    highest_power_of_two *= 2
  end
  return (total_elves - highest_power_of_two) * 2 + 1
end

total_elves = read_input("input.txt")
winner = find_winning_elf(total_elves)
puts winner
