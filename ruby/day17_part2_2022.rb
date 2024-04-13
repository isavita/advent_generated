require 'matrix'

ROCKSTR = <<~ROCKS
  ####

   # 
  ###
   # 

    #
    #
  ###

  #
  #
  #
  #

  ##
  ##
ROCKS

def main
  jet_pattern = File.read('input.txt').strip.each_byte.to_a

  rocks = get_rocks
  grid = {}
  (0...7).each { |x| grid[Vector[x, 0]] = true }
  floor, j = 0, 0
  repeat = {}

  i = 0
  curr = 0

  loop do
    key = [curr, j]
    if repeat.key?(key)
      previ, prev_floor = repeat[key]
      if (1_000_000_000_000 - i) % (i - previ) == 0
        puts floor + (1_000_000_000_000 - i) / (i - previ) * (floor - prev_floor)
        break
      end
    end
    repeat[key] = [i, floor]
    curr_rock = rocks[curr]
    pos = Vector[2, floor + 4]

    loop do
      jet = jet_pattern[j]
      j = (j + 1) % jet_pattern.size
      pos += dir_from_byte(jet)
      if collision(grid, curr_rock, pos)
        pos -= dir_from_byte(jet)
      end
      pos += Vector[0, -1]
      if collision(grid, curr_rock, pos)
        pos -= Vector[0, -1]
        curr_rock.each_key do |p|
          grid[p + pos] = true
          if (p + pos)[1] > floor
            floor = (p + pos)[1]
          end
        end
        break
      end
    end

    i += 1
    curr = (curr + 1) % rocks.size
  end
end

def collision(grid, rock, pos)
  rock.any? do |p, _|
    grid[p + pos] || (p + pos)[0] < 0 || (p + pos)[0] > 6
  end
end

def get_rocks
  ROCKSTR.split("\n\n").map do |rock|
    lines = rock.split("\n")
    lines.each_with_index.with_object({}) do |(line, y), hash|
      line.each_char.with_index do |char, x|
        hash[Vector[x, lines.size - 1 - y]] = true if char == '#'
      end
    end
  end
end

def dir_from_byte(b)
  case b.chr
  when 'N', 'U', '^' then Vector[0, 1]
  when 'E', 'R', '>' then Vector[1, 0]
  when 'S', 'D', 'v' then Vector[0, -1]
  when 'W', 'L', '<' then Vector[-1, 0]
  end
end

main