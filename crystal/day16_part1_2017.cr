file = File.open("input.txt")
moves = file.gets_to_end.chomp.split(',')

programs = ('a'..'p').to_a

moves.each do |move|
  case move[0]
  when 's'
    x = move[1..-1].to_i
    programs.rotate!(-x)
  when 'x'
    a, b = move[1..-1].split('/').map(&.to_i)
    programs[a], programs[b] = programs[b], programs[a]
  when 'p'
    a, b = move[1..-1].split('/').map { |c| c[0] }
    i = programs.index(a)
    j = programs.index(b)
    if i && j
      programs[i], programs[j] = programs[j], programs[i]
    end
  end
end

puts programs.join