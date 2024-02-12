
data = File.read("input.txt").chomp.split(",")
original = data.map { |s| s.to_i }

(0..99).each do |noun|
  (0..99).each do |verb|
    memory = original.dup
    memory[1] = noun
    memory[2] = verb
    if execute(memory) == 19690720
      puts 100 * noun + verb
      exit
    end
  end
end

def execute(memory)
  memory.each_slice(4) do |slice|
    case slice[0]
    when 1
      memory[slice[3]] = memory[slice[1]] + memory[slice[2]]
    when 2
      memory[slice[3]] = memory[slice[1]] * memory[slice[2]]
    when 99
      return memory[0]
    end
  end
  return memory[0]
end
