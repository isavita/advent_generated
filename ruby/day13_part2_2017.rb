
input = File.readlines('input.txt').map(&:chomp)

firewall = input.map { |line| line.split(': ').map(&:to_i) }.to_h

severity = 0
delay = 0

while true
  caught = false

  firewall.each do |depth, range|
    if (depth + delay) % ((range - 1) * 2) == 0
      caught = true
      break
    end
  end

  if !caught
    puts delay
    break
  end

  delay += 1
end
