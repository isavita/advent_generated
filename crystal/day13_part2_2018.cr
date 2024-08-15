require "file_utils"

class Cart
  property x, y, direction, turns

  def initialize(@x : Int32, @y : Int32, @direction : Char, @turns : Int32)
  end
end

class Position
  property x, y

  def initialize(@x : Int32, @y : Int32)
  end
end

def main
  file = File.read("input.txt")
  tracks = file.split("\n").map { |line| line.chars.to_a }
  carts = [] of Cart

  tracks.each_with_index do |track_line, y|
    track_line.each_with_index do |r, x|
      case r
      when '>', '<', '^', 'v'
        carts << Cart.new(x, y, r, 0)
        if r == '>' || r == '<'
          track_line[x] = '-'
        else
          track_line[x] = '|'
        end
      end
    end
  end

  while carts.size > 1
    carts.sort_by! { |cart| [cart.y, cart.x] }

    to_remove = Set(Int32).new
    carts.each_with_index do |cart, i|
      next if to_remove.includes?(i)

      move_cart(cart, tracks)
      if crash_index = check_crash(cart, carts)
        to_remove << i
        to_remove << crash_index
      end
    end

    carts.reject! { |cart| to_remove.includes?(carts.index(cart)) }
  end

  puts "#{carts[0].x},#{carts[0].y}"
end

def move_cart(cart : Cart, tracks : Array(Array(Char)))
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

def turn_cart(cart : Cart)
  case cart.turns % 3
  when 0
    case cart.direction
    when '>'
      cart.direction = '^'
    when '<'
      cart.direction = 'v'
    when '^'
      cart.direction = '<'
    when 'v'
      cart.direction = '>'
    end
  when 2
    case cart.direction
    when '>'
      cart.direction = 'v'
    when '<'
      cart.direction = '^'
    when '^'
      cart.direction = '>'
    when 'v'
      cart.direction = '<'
    end
  end
  cart.turns += 1
end

def change_direction(cart : Cart, track : Char)
  case track
  when '/'
    case cart.direction
    when '>'
      cart.direction = '^'
    when '<'
      cart.direction = 'v'
    when '^'
      cart.direction = '>'
    when 'v'
      cart.direction = '<'
    end
  when '\\'
    case cart.direction
    when '>'
      cart.direction = 'v'
    when '<'
      cart.direction = '^'
    when '^'
      cart.direction = '<'
    when 'v'
      cart.direction = '>'
    end
  end
end

def check_crash(cart : Cart, carts : Array(Cart)) : Int32?
  carts.each_with_index do |c, i|
    return i if c != cart && c.x == cart.x && c.y == cart.y
  end
  nil
end

main