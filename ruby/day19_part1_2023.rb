class Workflow
  attr_reader :name, :rules

  def initialize(name, rules_string)
    @name = name
    @rules = parse_rules(rules_string)
  end

  def process(part)
    @rules.each do |condition, destination|
      return destination if condition.nil? || eval_condition(condition, part)
    end
  end

  private

  def parse_rules(rules_string)
    rules_string.split(',').map do |rule|
      if rule.include?(':')
        condition, destination = rule.split(':')
        [condition, destination]
      else
        [nil, rule]
      end
    end
  end

  def eval_condition(condition, part)
    var, op, val = condition.match(/(\w+)([<>])(\d+)/).captures
    part[var].send(op, val.to_i)
  end
end

def parse_input(input)
  workflows, parts = input.split("\n\n")
  
  workflows = workflows.split("\n").map do |line|
    name, rules = line.match(/(\w+)\{(.*)\}/).captures
    [name, Workflow.new(name, rules)]
  end.to_h

  parts = parts.split("\n").map do |line|
    line.gsub(/[{}]/, '').split(',').map { |attr| 
      key, value = attr.split('=')
      [key, value.to_i]  # Convert value to integer
    }.to_h
  end

  [workflows, parts]
end

def process_part(part, workflows)
  current = 'in'
  while current != 'A' && current != 'R'
    current = workflows[current].process(part)
  end
  current == 'A'
end

def sum_accepted_parts(workflows, parts)
  parts.select { |part| process_part(part, workflows) }
       .sum { |part| part.values.sum }
end

# Read input from file
input = File.read('input.txt')

# Parse input and process parts
workflows, parts = parse_input(input)
result = sum_accepted_parts(workflows, parts)

puts "Sum of ratings for accepted parts: #{result}"
