
class Program
  property weight : Int32
  property holds : Array(String)

  def initialize(@weight, @holds = [] of String)
  end
end

def dfs(name : String, programs : Hash(String, Program)) : {Int32, Bool}
  program = programs[name]
  total_weight = program.weight

  weights = Hash(Int32, Int32).new(0)
  program.holds.each do |child|
    child_weight, balanced = dfs(child, programs)
    return {0, false} unless balanced
    total_weight += child_weight
    weights[child_weight] += 1
  end

  # Check for unbalance
  weights.each do |w1, c1|
    weights.each do |w2, c2|
      if w1 != w2 && c1 < c2
        unbalanced_program = program.holds.find do |child|
          child_weight, _ = dfs(child, programs)
          child_weight == w1
        end

        if unbalanced_program
          puts programs[unbalanced_program].weight + (w2 - w1)
          return {0, false}
        end
      end
    end
  end

  {total_weight, true}
end

def main
  # Read input
  lines = File.read_lines("input.txt")

  # Create data structure
  programs = {} of String => Program

  lines.each do |line|
    matches = line.scan(/[a-z]+|\d+/).map(&.[0])
    name = matches[0]
    weight = matches[1].to_i

    program = Program.new(weight, matches[2..] || [] of String)
    programs[name] = program
  end

  # Find unbalance
  root = "dtacyn" # Replace with root from Part One
  dfs(root, programs)
end

main
