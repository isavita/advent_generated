require "json"

def main
  input = File.read("input.txt").chomp
  puts some_assembly_required(input)
end

def some_assembly_required(input)
  wire_to_rule = {} of String => String

  input.split("\n").each do |inst|
    parts = inst.split(" -> ")
    wire_to_rule[parts[1]] = parts[0]
  end

  memo_dfs(wire_to_rule, "a", {} of String => Int32)
end

def memo_dfs(graph, entry, memo)
  return memo[entry] if memo.has_key?(entry)

  return entry.to_i if entry =~ /^\d+$/

  source_rule = graph[entry]
  parts = source_rule.split(" ")

  result = case parts.size
  when 1
    memo_dfs(graph, parts[0], memo)
  when 2
    ~(memo_dfs(graph, parts[1], memo))
  when 3
    case parts[1]
    when "AND"
      memo_dfs(graph, parts[0], memo) & memo_dfs(graph, parts[2], memo)
    when "OR"
      memo_dfs(graph, parts[0], memo) | memo_dfs(graph, parts[2], memo)
    when "LSHIFT"
      memo_dfs(graph, parts[0], memo) << memo_dfs(graph, parts[2], memo)
    when "RSHIFT"
      memo_dfs(graph, parts[0], memo) >> memo_dfs(graph, parts[2], memo)
    else
      raise "Unknown operation: #{parts[1]}"
    end
  else
    raise "Invalid instruction format: #{source_rule}"
  end

  memo[entry] = result
  result
end

main