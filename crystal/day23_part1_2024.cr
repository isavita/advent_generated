
def solve(input : String) : Int32
  connections = Hash(String, Set(String)).new { |h, k| h[k] = Set(String).new }
  
  input.each_line do |line|
    a, b = line.strip.split("-")
    connections[a].add(b)
    connections[b].add(a)
  end

  count = 0
  computers = connections.keys.to_a.sort
  
  computers.each_with_index do |a, i|
    (i+1...computers.size).each do |j|
      b = computers[j]
      next unless connections[a].includes?(b)
      
      (j+1...computers.size).each do |k|
        c = computers[k]
        next unless connections[a].includes?(c) && connections[b].includes?(c)

        if [a,b,c].any? { |x| x.starts_with?('t') }
          count += 1
        end
      end
    end
  end
  
  count
end

input = File.read("input.txt")
puts solve(input)
