def swap_position(str, x, y)
  str[x], str[y] = str[y], str[x]
  str
end

def swap_letter(str, x, y)
  str.tr(x+y, y+x)
end

def rotate(str, direction, steps)
  steps %= str.length
  direction == 'left' ? str.chars.rotate(steps).join : str.chars.rotate(-steps).join
end

def rotate_based_on_letter(str, x)
  index = str.index(x)
  rotations = 1 + index + (index >= 4 ? 1 : 0)
  rotate(str, 'right', rotations)
end

def reverse_positions(str, x, y)
  str[x..y] = str[x..y].reverse
  str
end

def move_position(str, x, y)
  char = str.slice!(x)
  str.insert(y, char)
end

password = "abcdefgh"

File.readlines('input.txt').each do |line|
  case line
  when /swap position (\d+) with position (\d+)/
    password = swap_position(password, $1.to_i, $2.to_i)
  when /swap letter (\w) with letter (\w)/
    password = swap_letter(password, $1, $2)
  when /rotate (left|right) (\d+) step/
    password = rotate(password, $1, $2.to_i)
  when /rotate based on position of letter (\w)/
    password = rotate_based_on_letter(password, $1)
  when /reverse positions (\d+) through (\d+)/
    password = reverse_positions(password, $1.to_i, $2.to_i)
  when /move position (\d+) to position (\d+)/
    password = move_position(password, $1.to_i, $2.to_i)
  end
end

puts password
