
input = File.read('input.txt').strip

programs = ('a'..'p').to_a

input.split(',').each do |move|
  case move[0]
  when 's'
    programs.rotate!(-move[1..-1].to_i)
  when 'x'
    a, b = move[1..-1].split('/').map(&:to_i)
    programs[a], programs[b] = programs[b], programs[a]
  when 'p'
    a, b = move[1..-1].split('/')
    a_index = programs.index(a)
    b_index = programs.index(b)
    programs[a_index], programs[b_index] = programs[b_index], programs[a_index]
  end
end

puts programs.join
