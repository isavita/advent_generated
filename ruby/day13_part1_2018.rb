
def move_cart(cart, track)
  x, y, dir, turn = cart
  case dir
  when '^'
    y -= 1
    case track[y][x]
    when '\\'
      dir = '<'
    when '/'
      dir = '>'
    when '+'
      dir, turn = turn_cart(dir, turn)
    end
  when 'v'
    y += 1
    case track[y][x]
    when '\\'
      dir = '>'
    when '/'
      dir = '<'
    when '+'
      dir, turn = turn_cart(dir, turn)
    end
  when '<'
    x -= 1
    case track[y][x]
    when '\\'
      dir = '^'
    when '/'
      dir = 'v'
    when '+'
      dir, turn = turn_cart(dir, turn)
    end
  when '>'
    x += 1
    case track[y][x]
    when '\\'
      dir = 'v'
    when '/'
      dir = '^'
    when '+'
      dir, turn = turn_cart(dir, turn)
    end
  end
  [x, y, dir, turn]
end

def turn_cart(dir, turn)
  turns = { '^' => ['<', '^', '>'], 'v' => ['>', 'v', '<'], '<' => ['v', '<', '^'], '>' => ['^', '>', 'v'] }
  new_dir = turns[dir][turn]
  new_turn = (turn + 1) % 3
  [new_dir, new_turn]
end

def find_crash(track, carts)
  loop do
    carts.sort_by! { |cart| [cart[1], cart[0]] }
    carts.each_with_index do |cart, index|
      carts[index] = move_cart(cart, track)
      if carts.map { |c| [c[0], c[1]] }.count(carts[index][0..1]) > 1
        return carts[index][0..1].join(',')
      end
    end
  end
end

track = []
carts = []

File.readlines('input.txt').each_with_index do |line, y|
  track[y] = line.chomp.chars
  line.chars.each_with_index do |char, x|
    if char.match?(/[v^<>]/)
      dir = char
      carts << [x, y, dir, 0]
      track[y][x] = dir.match?(/[<>]/) ? '-' : '|'
    end
  end
end

puts find_crash(track, carts)
