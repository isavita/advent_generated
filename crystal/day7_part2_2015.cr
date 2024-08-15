require "file"

def main
  input = File.read("input.txt").strip
  puts some_assembly_required(input)
end

def some_assembly_required(input)
  wire_to_rule = {} of String => String

  input.lines.each do |inst|
    parts = inst.split(" -> ")
    wire_to_rule[parts[1]] = parts[0]
  end

  a_signal = memo_dfs(wire_to_rule, "a", {} of String => Int32)

  wire_to_rule["b"] = a_signal.to_s
  memo_dfs(wire_to_rule, "a", {} of String => Int32)
end

def memo_dfs(graph, entry, memo)
  return memo[entry] if memo.has_key?(entry)

  return entry.to_i if entry =~ /\A\d+\z/

  source_rule = graph[entry]
  return 0 unless source_rule

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
      0 # Default case if the operation is unknown
    end
  else
    0 # Default case if the parts size is unexpected
  end

  memo[entry] = result
  result
end

main