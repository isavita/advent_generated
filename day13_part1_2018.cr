
struct Cart
  property x : Int32
  property y : Int32
  property dir : Char
  property turn : Int32

  def initialize(@x : Int32, @y : Int32, @dir : Char, @turn : Int32)
  end
end

def moving_down(track : Array(Array(Char)), cart : Cart) : Cart
  case track[cart.y + 1][cart.x]
  when '/'
    cart.dir = '<'
  when '\\'
    cart.dir = '>'
  when '+'
    if cart.turn == 0
      cart.dir = '>'
      cart.turn = 1
    elsif cart.turn == 1
      cart.turn = 2
    elsif cart.turn == 2
      cart.dir = '<'
      cart.turn = 0
    end
  end
  cart.y += 1
  cart
end

def moving_up(track : Array(Array(Char)), cart : Cart) : Cart
  case track[cart.y - 1][cart.x]
  when '/'
    cart.dir = '>'
  when '\\'
    cart.dir = '<'
  when '+'
    if cart.turn == 0
      cart.dir = '<'
      cart.turn = 1
    elsif cart.turn == 1
      cart.turn = 2
    elsif cart.turn == 2
      cart.dir = '>'
      cart.turn = 0
    end
  end
  cart.y -= 1
  cart
end

def moving_left(track : Array(Array(Char)), cart : Cart) : Cart
  case track[cart.y][cart.x - 1]
  when '/'
    cart.dir = 'v'
  when '\\'
    cart.dir = '^'
  when '+'
    if cart.turn == 0
      cart.dir = 'v'
      cart.turn = 1
    elsif cart.turn == 1
      cart.turn = 2
    elsif cart.turn == 2
      cart.dir = '^'
      cart.turn = 0
    end
  end
  cart.x -= 1
  cart
end

def moving_right(track : Array(Array(Char)), cart : Cart) : Cart
  case track[cart.y][cart.x + 1]
  when '\\'
    cart.dir = 'v'
  when '/'
    cart.dir = '^'
  when '+'
    if cart.turn == 0
      cart.dir = '^'
      cart.turn = 1
    elsif cart.turn == 1
      cart.turn = 2
    elsif cart.turn == 2
      cart.dir = 'v'
      cart.turn = 0
    end
  end
  cart.x += 1
  cart
end

input = File.read("input.txt")
lines = input.split("\n")
track = [] of Array(Char)
carts = [] of Cart

lines.each_with_index do |line, i|
  track.push [] of Char
  line.each_char.with_index do |s, j|
    case s
    when '>'
      track[i].push('-')
      carts.push Cart.new(j, i, '>', 0)
    when '<'
      track[i].push('-')
      carts.push Cart.new(j, i, '<', 0)
    when '^'
      track[i].push('|')
      carts.push Cart.new(j, i, '^', 0)
    when 'v'
      track[i].push('|')
      carts.push Cart.new(j, i, 'v', 0)
    else
      track[i].push(s)
    end
  end
end

collision = false
until collision
  carts.each_with_index do |cart, i|
    case cart.dir
    when '>'
      carts[i] = moving_right(track, cart)
    when '<'
      carts[i] = moving_left(track, cart)
    when '^'
      carts[i] = moving_up(track, cart)
    when 'v'
      carts[i] = moving_down(track, cart)
    else
      puts "error not valid cart"
    end
  end

  carts.each_with_index do |cart, i|
    (i + 1...carts.size).each do |j|
      if cart.x == carts[j].x && cart.y == carts[j].y
        collision = true
        puts "#{cart.x},#{cart.y}"
      end
    end
  end
end
