class Cart
  attr_accessor :x, :y, :direction, :turns

  def initialize(x, y, direction)
    @x, @y, @direction, @turns = x, y, direction, 0
  end
end

def move_cart(cart, tracks)
  case cart.direction
  when '>'
    cart.x += 1
  when '<'
    cart.x -= 1
  when '^'
    cart.y -= 1
  when 'v'
    cart.y += 1
  end

  case tracks[cart.y][cart.x]
  when '+'
    turn_cart(cart)
  when '/', '\\'
    change_direction(cart, tracks[cart.y][cart.x])
  end
end

def turn_cart(cart)
  case cart.turns % 3
  when 0
    cart.direction = {'>' => '^', '<' => 'v', '^' => '<', 'v' => '>'}[cart.direction]
  when 2
    cart.direction = {'>' => 'v', '<' => '^', '^' => '>', 'v' => '<'}[cart.direction]
  end
  cart.turns += 1
end

def change_direction(cart, track)
  if track == '/'
    cart.direction = {'>' => '^', '<' => 'v', '^' => '>', 'v' => '<'}[cart.direction]
  elsif track == '\\'
    cart.direction = {'>' => 'v', '<' => '^', '^' => '<', 'v' => '>'}[cart.direction]
  end
end

def check_crash(cart, carts)
  carts.each_with_index do |c, i|
    return i if c != cart && c.x == cart.x && c.y == cart.y
  end
  -1
end

tracks = []
carts = []

File.readlines('input.txt').each_with_index do |line, y|
  track_line = []
  line.chomp.each_char.with_index do |ch, x|
    if ['>', '<', '^', 'v'].include?(ch)
      carts << Cart.new(x, y, ch)
      track_line << (['>', '<'].include?(ch) ? '-' : '|')
    else
      track_line << ch
    end
  end
  tracks << track_line
end

while carts.size > 1
  carts.sort_by! { |cart| [cart.y, cart.x] }
  to_remove = {}

  carts.each_with_index do |cart, i|
    next if to_remove.key?(i)

    move_cart(cart, tracks)
    crash_index = check_crash(cart, carts)
    if crash_index != -1
      to_remove[i] = true
      to_remove[crash_index] = true
    end
  end

  carts = carts.each_with_index.reject { |cart, i| to_remove.key?(i) }.map(&:first)
end

puts "#{carts[0].x},#{carts[0].y}"