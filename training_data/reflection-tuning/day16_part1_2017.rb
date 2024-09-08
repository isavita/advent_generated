# Read input from file
moves = File.read('input.txt').strip.split(',')

# Initialize program order
programs = ('a'..'p').to_a.join

# Process each move
moves.each do |move|
  case move[0]
  when 's'
    x = move[1..-1].to_i
    programs = programs[-x..-1] + programs[0...-x]
  when 'x'
    a, b = move[1..-1].split('/').map(&:to_i)
    programs[a], programs[b] = programs[b], programs[a]
  when 'p'
    a, b = move[1..-1].split('/')
    i, j = programs.index(a), programs.index(b)
    programs[i], programs[j] = programs[j], programs[i]
  end
end

# Print the final order
puts programs
