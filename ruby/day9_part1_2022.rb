
head = { x: 0, y: 0 }
tail = { x: 0, y: 0 }
visited = { tail => true }

File.open("input.txt").each do |line|
  dir, steps = line.split(" ")
  num_steps = steps.to_i

  num_steps.times do
    case dir
    when "R"
      head[:x] += 1
    when "L"
      head[:x] -= 1
    when "U"
      head[:y] += 1
    when "D"
      head[:y] -= 1
    end

    if (head[:x] - tail[:x]).abs > 1 || (head[:y] - tail[:y]).abs > 1
      if head[:x] != tail[:x] && head[:y] != tail[:y]
        tail[:x] += head[:x] > tail[:x] ? 1 : -1
        tail[:y] += head[:y] > tail[:y] ? 1 : -1
      else
        tail[:x] += head[:x] > tail[:x] ? 1 : head[:x] < tail[:x] ? -1 : 0
        tail[:y] += head[:y] > tail[:y] ? 1 : head[:y] < tail[:y] ? -1 : 0
      end
    end

    visited[tail] = true
  end
end

puts visited.length
