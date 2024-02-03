
input = File.read('input.txt').split("\n")

pipes = {}
input.each do |line|
  parts = line.split(" <-> ")
  pipes[parts[0].to_i] = parts[1].split(',').map(&:to_i)
end

def find_group(pipes, start)
  group = [start]
  queue = [start]
  
  while queue.any?
    current = queue.shift
    pipes[current].each do |neighbor|
      next if group.include?(neighbor)
      group << neighbor
      queue << neighbor
    end
  end
  
  group
end

group0 = find_group(pipes, 0)
puts group0.size

groups = []
pipes.keys.each do |program|
  found = false
  groups.each do |group|
    if group.include?(program)
      found = true
      break
    end
  end
  
  next if found
  
  new_group = find_group(pipes, program)
  groups << new_group
end

puts groups.size
