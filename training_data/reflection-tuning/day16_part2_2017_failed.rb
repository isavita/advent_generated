def spin(programs, x)
  programs[-x..-1] + programs[0...-x]
end

def exchange(programs, a, b)
  programs[a], programs[b] = programs[b], programs[a]
  programs
end

def partner(programs, a, b)
  i, j = programs.index(a), programs.index(b)
  programs[i], programs[j] = programs[j], programs[i]
  programs
end

def dance(programs, moves)
  moves.each do |move|
    case move[0]
    when 's'
      programs = spin(programs, move[1..-1].to_i)
    when 'x'
      a, b = move[1..-1].split('/').map(&:to_i)
      programs = exchange(programs, a, b)
    when 'p'
      a, b = move[1..-1].split('/')
      programs = partner(programs, a, b)
    end
  end
  programs
end

programs = ('a'..'p').to_a.join
moves = File.read('input.txt').strip.split(',')

# Part One
part_one = dance(programs, moves)
puts "Part One: #{part_one}"

# Part Two
seen = {}
cycle_start = 0
cycle_length = 0
billion = 1_000_000_000

billion.times do |i|
  if seen[programs]
    cycle_start = seen[programs]
    cycle_length = i - cycle_start
    break
  end
  seen[programs] = i
  programs = dance(programs, moves)
end

remaining = (billion - cycle_start) % cycle_length
remaining.times do
  programs = dance(programs, moves)
end

puts "Part Two: #{programs}"
