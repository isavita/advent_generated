
require "regex"

class Rule
  property resolved = [] of String
  property options = [] of Array(Int32)

  def to_s
    "OPTIONS: #{options}\nRESOLVED: #{resolved}"
  end
end

def parse_input(input)
  parts = input.split("\n\n")
  rules = {} of Int32 => Rule

  parts[0].split('\n').each do |r|
    if r =~ /[a-z]/
      num, char = r.scan(/(\d+): "([a-z])"/).first.captures
      rules[num.not_nil!.to_i] = Rule.new.tap { |rule| rule.resolved = [char.not_nil!] }
    else
      key, rule_str = r.split(": ")
      new_rule = Rule.new
      rule_str.split(" | ").each do |rule_nums|
        option = rule_nums.split.map(&.to_i)
        new_rule.options << option
      end
      rules[key.to_i] = new_rule
    end
  end

  messages = parts[1].split('\n')
  {rules, messages}
end

def fill_in_graph(graph, entry)
  return graph[entry].resolved.dup if graph[entry].resolved.any?

  graph[entry].options.each do |option|
    resolved = [""]
    option.each do |entry_point|
      nested_resolve_vals = fill_in_graph(graph, entry_point)
      new_resolved = [] of String
      nested_resolve_vals.each do |next_piece|
        resolved.each do |prev|
          new_resolved << prev + next_piece
        end
      end
      resolved = new_resolved
    end
    graph[entry].resolved.concat(resolved)
  end

  graph[entry].resolved
end

def solve(input)
  graph, messages = parse_input(input)

  fill_in_graph(graph, 42)
  fill_in_graph(graph, 31)

  part42 = "(#{graph[42].resolved.join("|")})"
  part31 = "(#{graph[31].resolved.join("|")})"

  rule8_string = "(#{part42})+"

  match_rule_zero = messages.count do |m|
    (1...10).any? do |num|
      pattern = Regex.new("^#{rule8_string}#{part42}{#{num}}#{part31}{#{num}}$")
      pattern.matches?(m)
    end
  end

  match_rule_zero
end

input = File.read("input.txt").strip
puts solve(input)
