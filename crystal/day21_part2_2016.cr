
def swap_position(str : String, x : Int32, y : Int32) : String
  arr = str.chars
  arr[x], arr[y] = arr[y], arr[x]
  arr.join
end

def swap_letter(str : String, x : Char, y : Char) : String
  str.gsub(x, "*").gsub(y, x.to_s).gsub("*", y.to_s)
end

def rotate_left(str : String, x : Int32) : String
  x %= str.size
  str[x..] + str[0...x]
end

def rotate_right(str : String, x : Int32) : String
  x %= str.size
  str[str.size - x..] + str[0...str.size - x]
end

def rotate_based(str : String, x : Char) : String
  index = str.index(x) || raise "Letter not found"
  rotations = 1 + index + (index >= 4 ? 1 : 0)
  rotate_right(str, rotations)
end

def reverse_positions(str : String, x : Int32, y : Int32) : String
  arr = str.chars
  arr[x..y] = arr[x..y].reverse
  arr.join
end

def move_position(str : String, x : Int32, y : Int32) : String
  char = str[x]
  new_str = str.delete_at(x)
  new_str.insert(y, char)
end

def unscramble_rotate_based(str : String, x : Char) : String
  original_length = str.size
  (0..original_length-1).each do |i|
    temp_str = str
    rotations = 1 + i + (i >= 4 ? 1 : 0)
    
    candidate_str = rotate_left(temp_str, rotations)
    if candidate_str.index(x) == i
      return candidate_str
    end
  end
  raise "Could not unscramble rotate_based"
end



def scramble(str : String, instructions : Array(String)) : String
  result = str
  instructions.each do |instruction|
    case instruction
    when /swap position (\d+) with position (\d+)/
      result = swap_position(result, $1.to_i, $2.to_i)
    when /swap letter (\w) with letter (\w)/
      result = swap_letter(result, $1[0], $2[0])
    when /rotate left (\d+) steps?/
      result = rotate_left(result, $1.to_i)
    when /rotate right (\d+) steps?/
      result = rotate_right(result, $1.to_i)
    when /rotate based on position of letter (\w)/
      result = rotate_based(result, $1[0])
    when /reverse positions (\d+) through (\d+)/
      result = reverse_positions(result, $1.to_i, $2.to_i)
    when /move position (\d+) to position (\d+)/
      result = move_position(result, $1.to_i, $2.to_i)
    else
      puts "Unknown instruction: #{instruction}"
    end
  end
  result
end

def unscramble(str : String, instructions : Array(String)) : String
  result = str
  instructions.reverse_each do |instruction|
    case instruction
    when /swap position (\d+) with position (\d+)/
      result = swap_position(result, $1.to_i, $2.to_i)
    when /swap letter (\w) with letter (\w)/
      result = swap_letter(result, $1[0], $2[0])
    when /rotate left (\d+) steps?/
      result = rotate_right(result, $1.to_i)
    when /rotate right (\d+) steps?/
      result = rotate_left(result, $1.to_i)
    when /rotate based on position of letter (\w)/
        result = unscramble_rotate_based(result, $1[0])
    when /reverse positions (\d+) through (\d+)/
      result = reverse_positions(result, $1.to_i, $2.to_i)
    when /move position (\d+) to position (\d+)/
      result = move_position(result, $2.to_i, $1.to_i)
    else
      puts "Unknown instruction: #{instruction}"
    end
  end
  result
end


def main
  instructions = File.read("input.txt").lines.map &.strip

  # Part 1
  scrambled_password = scramble("abcdefgh", instructions)
  puts "Scrambled password: #{scrambled_password}"

  # Part 2
  unscrambled_password = unscramble("fbgdceah", instructions)
  puts "Unscrambled password: #{unscrambled_password}"
end

main
