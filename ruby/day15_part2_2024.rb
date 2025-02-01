
UP = 0 - 1i
DOWN = 0 + 1i
LEFT = -1 + 0i
RIGHT = 1 + 0i

def solve(input)
    m, steps = parse(input)
    robot = m.key('@')
    steps.each do |dir|
      try_to_step(m, robot, dir) && (robot += dir)
    end
    m.sum { |k, v| v == '[' || v == 'O' ? k.real + 100 * k.imag : 0 }
end

def try_to_step(m, pos, dir)
    orig = m.dup
    case m[pos]
    when '.'
      return true
    when 'O', '@'
        if try_to_step(m, pos + dir, dir)
          m[pos + dir] = m[pos]
          m[pos] = '.'
          return true
        end
    when ']'
        if try_to_step(m, pos + LEFT, dir)
           return true
        end
    when '['
      if dir == LEFT
        if try_to_step(m, pos + LEFT, dir)
          m[pos+LEFT] = '['
          m[pos] = ']'
          m[pos+RIGHT] = '.'
          return true
        end
      elsif dir == RIGHT
        if try_to_step(m, pos + 2 * RIGHT, dir)
          m[pos] = '.'
          m[pos+RIGHT] = '['
          m[pos+2*RIGHT] = ']'
          return true
        end
      else
        if try_to_step(m, pos + dir, dir) && try_to_step(m, pos + RIGHT + dir, dir)
            m[pos] = '.'
            m[pos+RIGHT] = '.'
            m[pos+dir] = '['
            m[pos+dir+RIGHT] = ']'
            return true
          end
      end
    end
    m.replace(orig)
    false
end


def scale_up(input)
  input.gsub('#', '##')
       .gsub('.', '..')
       .gsub('O', '[]')
       .gsub('@', '@.')
end


def parse(input)
    blocks = input.strip.split("\n\n")
    lines = blocks[0].split("\n")
    m = {}
    lines.each_with_index do |line, y|
      line.chars.each_with_index do |char, x|
        m[x + y * 1i] = char
      end
    end
    steps = blocks[1].gsub("\n", '').chars.map do |ch|
        case ch
        when '^' then UP
        when '<' then LEFT
        when '>' then RIGHT
        when 'v' then DOWN
        end
    end
  [m, steps]
end

input = File.read("input.txt")
puts solve(input)
puts solve(scale_up(input))
