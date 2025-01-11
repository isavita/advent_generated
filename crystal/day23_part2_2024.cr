
def solve
  connections = Hash(String, Set(String)).new { |h, k| h[k] = Set(String).new }
  File.open("./input.txt") do |file|
    file.each_line do |line|
      a, b = line.strip.split("-")
      connections[a] << b
      connections[b] << a
    end
  end

  nodes = connections.keys.sort

  # Part 1
  count_t = 0
  nodes.each_with_index do |a, i|
    nodes[i + 1..-1].each do |b|
      next unless connections[a].includes?(b)
      nodes[i + 2..-1].each do |c|
        next unless connections[a].includes?(c) && connections[b].includes?(c)
        count_t += 1 if a.starts_with?('t') || b.starts_with?('t') || c.starts_with?('t')
      end
    end
  end
  puts "Part 1: #{count_t}"

  # Part 2
  max_clique = [] of String
  nodes.each do |node|
    clique = [node]
    nodes.each do |other|
      next if node == other
      if clique.all? { |n| connections[n].includes?(other) }
        clique << other
      end
    end
    
    
    valid = true
    clique.each do |n1|
      clique.each do |n2|
        if n1 != n2 && !connections[n1].includes?(n2)
          valid = false
          break
        end
      end
      break unless valid
    end

    if valid && clique.size > max_clique.size
      max_clique = clique
    end
  end

  puts "Part 2: #{max_clique.sort.join(",")}"
end

solve
