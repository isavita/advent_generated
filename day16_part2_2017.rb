
def dance(moves, start_pos)
  programs = start_pos.chars
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
      a_index, b_index = programs.index(a), programs.index(b)
      programs[a_index], programs[b_index] = programs[b_index], programs[a_index]
    end
  end
  programs.join
end

moves = File.read('input.txt').strip.split(',')

# Part 1
start_pos = ('a'..'p').to_a.join
puts dance(moves, start_pos)

# Part 2
seen = {start_pos => 0}
current_pos = start_pos
1.upto(1_000_000_000) do |i|
  current_pos = dance(moves, current_pos)
  if seen[current_pos]
    cycle_length = i - seen[current_pos]
    remaining_loops = (1_000_000_000 - i) % cycle_length
    puts seen.key(seen.values.find { |v| v == seen[current_pos] + remaining_loops })
    break
  else
    seen[current_pos] = i
  end
end
