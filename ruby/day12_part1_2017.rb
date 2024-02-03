
input = File.readlines('input.txt').map(&:chomp)

pipes = {}
input.each do |line|
  parts = line.split(' <-> ')
  pipes[parts[0].to_i] = parts[1].split(', ').map(&:to_i)
end

def find_group(program, pipes, group)
  return if group.include?(program)

  group << program
  pipes[program].each do |p|
    find_group(p, pipes, group)
  end
end

group = []
find_group(0, pipes, group)
puts group.size
