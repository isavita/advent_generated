
def dfs(name, programs)
  program = programs[name]
  total_weight = program[:weight]

  weights = Hash.new(0)
  program[:holds].each do |child|
    weight, balanced = dfs(child, programs)
    return [0, false] unless balanced
    total_weight += weight
    weights[weight] += 1
  end

  weights.each do |w1, c1|
    weights.each do |w2, c2|
      if w1 != w2 && c1 < c2
        unbalanced_program = program[:holds].find do |child|
          child_weight, = dfs(child, programs)
          child_weight == w1
        end
        puts(programs[unbalanced_program][:weight] + (w2 - w1))
        return [0, false]
      end
    end
  end
  [total_weight, true]
end

input = File.read('input.txt')
lines = input.strip.split("\n")

programs = {}
lines.each do |line|
  matches = line.scan(/[a-z]+|\d+/)
  name = matches[0]
  weight = matches[1].to_i
  holds = matches.drop(2)
  programs[name] = { weight: weight, holds: holds }
end

root = "dtacyn" # Replace this with the root found in Part One
dfs(root, programs)
