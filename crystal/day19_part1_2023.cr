
class Part
  property x : Int32
  property m : Int32
  property a : Int32
  property s : Int32

  def initialize(@x, @m, @a, @s)
  end

  def total_rating
    @x + @m + @a + @s
  end
end

class Workflow
  property name : String
  property rules : Array(Rule)

  def initialize(@name, @rules)
  end

  def process(part : Part) : String
    @rules.each do |rule|
      result = rule.apply(part)
      return result if result
    end
    raise "No rule matched"
  end
end

class Rule
  def initialize(@condition : Proc(Part, Bool)?, @destination : String)
  end

  def apply(part : Part) : String?
    if @condition.nil? || @condition.not_nil!.call(part)
      @destination
    else
      nil
    end
  end
end

def parse_workflows(workflow_lines : Array(String)) : Hash(String, Workflow)
  workflows = {} of String => Workflow
  workflow_lines.each do |line|
    name, rules_str = line.split('{')
    rules_str = rules_str.chomp('}')
    rules = rules_str.split(',').map do |rule_str|
      if rule_str.includes?(':')
        condition_str, destination = rule_str.split(':')
        category = condition_str[0]
        comparator = condition_str[1]
        value = condition_str[2..-1].to_i

        condition = case {category, comparator}
        when {'x', '<'}
          ->(part : Part) { part.x < value }
        when {'x', '>'}
          ->(part : Part) { part.x > value }
        when {'m', '<'}
          ->(part : Part) { part.m < value }
        when {'m', '>'}
          ->(part : Part) { part.m > value }
        when {'a', '<'}
          ->(part : Part) { part.a < value }
        when {'a', '>'}
          ->(part : Part) { part.a > value }
        when {'s', '<'}
          ->(part : Part) { part.s < value }
        when {'s', '>'}
          ->(part : Part) { part.s > value }
        else
          raise "Invalid condition"
        end

        Rule.new(condition, destination)
      else
        Rule.new(nil, rule_str)
      end
    end
    workflows[name] = Workflow.new(name, rules)
  end
  workflows
end

def parse_parts(part_lines : Array(String)) : Array(Part)
  part_lines.map do |line|
    ratings = line.chomp('}').lchop('{').split(',').map do |rating|
      rating.split('=')[1].to_i
    end
    Part.new(ratings[0], ratings[1], ratings[2], ratings[3])
  end
end

def process_part(part : Part, workflows : Hash(String, Workflow)) : Bool
  current_workflow = "in"
  while current_workflow != "A" && current_workflow != "R"
    current_workflow = workflows[current_workflow].process(part)
  end
  current_workflow == "A"
end

def solve(input : String) : Int32
  workflow_lines, part_lines = input.split("\n\n").map(&.lines)
  workflows = parse_workflows(workflow_lines)
  parts = parse_parts(part_lines)

  parts.select { |part| process_part(part, workflows) }
    .sum(&.total_rating)
end

# Read input from file
input = File.read("input.txt")
puts solve(input)
