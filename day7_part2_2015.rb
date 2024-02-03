
input = File.read("input.txt").strip

def some_assembly_required(input)
  wire_to_rule = {}

  input.split("\n").each do |inst|
    parts = inst.split(" -> ")
    wire_to_rule[parts[1]] = parts[0]
  end

  a_signal = memo_dfs(wire_to_rule, "a", {})

  wire_to_rule["b"] = a_signal.to_s
  memo_dfs(wire_to_rule, "a", {})
end

def memo_dfs(graph, entry, memo)
  return memo[entry] if memo.key?(entry)

  if entry.match?(/[0-9]/)
    return entry.to_i
  end

  source_rule = graph[entry]
  parts = source_rule.split(" ")

  result = case
           when parts.length == 1
             memo_dfs(graph, parts[0], memo)
           when parts[0] == "NOT"
             start = memo_dfs(graph, parts[1], memo)
             (0xFFFF ^ start)
           when parts[1] == "AND"
             memo_dfs(graph, parts[0], memo) & memo_dfs(graph, parts[2], memo)
           when parts[1] == "OR"
             memo_dfs(graph, parts[0], memo) | memo_dfs(graph, parts[2], memo)
           when parts[1] == "LSHIFT"
             memo_dfs(graph, parts[0], memo) << memo_dfs(graph, parts[2], memo)
           when parts[1] == "RSHIFT"
             memo_dfs(graph, parts[0], memo) >> memo_dfs(graph, parts[2], memo)
           end

  memo[entry] = result
  result
end

puts some_assembly_required(input)
