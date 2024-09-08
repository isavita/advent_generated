def dance(programs, moves)
  moves.each do |move|
    case move[0]
    when 's'
      x = move[1..-1].to_i
      programs.rotate!(-x)
    when 'x'
      a, b = move[1..-1].split('/').map(&:to_i)
      programs[a], programs[b] = programs[b], programs[a]
    when 'p'
      a, b = move[1..-1].split('/')
      i, j = programs.index(a), programs.index(b)
      programs[i], programs[j] = programs[j], programs[i]
    end
  end
  programs
end

input = File.read('input.txt').strip.split(',')
programs = ('a'..'p').to_a

# Part One
part_one = dance(programs.dup, input).join

# Part Two
seen = {}
programs = ('a'..'p').to_a
iterations = 1_000_000_000

iterations.times do |i|
  key = programs.join
  if seen[key]
    cycle_length = i - seen[key]
    remaining = (iterations - i) % cycle_length
    remaining.times { programs = dance(programs, input) }
    break
  end
  seen[key] = i
  programs = dance(programs, input)
end

part_two = programs.join

puts "Part One: #{part_one}"
puts "Part Two: #{part_two}"
