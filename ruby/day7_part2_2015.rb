
def some_assembly_required(input)
  wire_to_rule = {}
  input.each_line do |line|
    parts = line.strip.split(' -> ')
    wire_to_rule[parts[1]] = parts[0]
  end

  a_signal = memo_dfs(wire_to_rule, 'a', {})
  wire_to_rule['b'] = a_signal.to_s
  memo_dfs(wire_to_rule, 'a', {})
end

def memo_dfs(graph, entry, memo)
  return memo[entry] if memo.key?(entry)
  return entry.to_i if entry =~ /\A\d+\z/

  source_rule = graph[entry]
  parts = source_rule.split

  result = case parts.size
  when 1
    memo_dfs(graph, parts[0], memo)
  when 2
    ~memo_dfs(graph, parts[1], memo) & 0xFFFF
  when 3
    case parts[1]
    when 'AND'
      memo_dfs(graph, parts[0], memo) & memo_dfs(graph, parts[2], memo)
    when 'OR'
      memo_dfs(graph, parts[0], memo) | memo_dfs(graph, parts[2], memo)
    when 'LSHIFT'
      (memo_dfs(graph, parts[0], memo) << parts[2].to_i) & 0xFFFF
    when 'RSHIFT'
      memo_dfs(graph, parts[0], memo) >> parts[2].to_i
    end
  end

  memo[entry] = result
end

input = File.read('input.txt').strip
puts some_assembly_required(input)
