class Scrambler
  def initialize(instructions)
    @instructions = instructions
  end

  def scramble(password)
    @instructions.each do |instruction|
      case instruction
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
    password
  end

  def unscramble(password)
    @instructions.reverse.each do |instruction|
      case instruction
      when /swap position (\d+) with position (\d+)/
        password = swap_position(password, $1.to_i, $2.to_i)
      when /swap letter (\w) with letter (\w)/
        password = swap_letter(password, $1, $2)
      when /rotate (left|right) (\d+) step/
        password = rotate(password, $1 == 'left' ? 'right' : 'left', $2.to_i)
      when /rotate based on position of letter (\w)/
        password = unrotate_based_on_letter(password, $1)
      when /reverse positions (\d+) through (\d+)/
        password = reverse_positions(password, $1.to_i, $2.to_i)
      when /move position (\d+) to position (\d+)/
        password = move_position(password, $2.to_i, $1.to_i)
      end
    end
    password
  end

  private

  def swap_position(password, x, y)
    password[x], password[y] = password[y], password[x]
    password
  end

  def swap_letter(password, x, y)
    password.tr(x+y, y+x)
  end

  def rotate(password, direction, steps)
    steps %= password.length
    direction == 'right' ? password.chars.rotate(-steps).join : password.chars.rotate(steps).join
  end

  def rotate_based_on_letter(password, letter)
    index = password.index(letter)
    rotations = 1 + index + (index >= 4 ? 1 : 0)
    rotate(password, 'right', rotations)
  end

  def reverse_positions(password, x, y)
    password[x..y] = password[x..y].reverse
    password
  end

  def move_position(password, x, y)
    char = password.slice!(x)
    password.insert(y, char)
  end

  def unrotate_based_on_letter(password, letter)
    original_password = password
    password.length.times do
      if rotate_based_on_letter(password, letter) == original_password
        return password
      end
      password = rotate(password, 'left', 1)
    end
    password
  end
end

# Read instructions from file
instructions = File.readlines('input.txt').map(&:chomp)

scrambler = Scrambler.new(instructions)

# Part One
part_one_result = scrambler.scramble('abcdefgh')
puts "Part One: #{part_one_result}"

# Part Two
part_two_result = scrambler.unscramble('fbgdceah')
puts "Part Two: #{part_two_result}"
