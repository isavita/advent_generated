class Node
  attr_accessor :value, :next, :prev
  def initialize(value)
    @value = value
    @next = nil
    @prev = nil
  end
end

def mix(numbers, rounds = 1)
  nodes = numbers.map { |n| Node.new(n) }
  nodes.each_with_index do |node, i|
    node.next = nodes[(i + 1) % nodes.size]
    node.prev = nodes[(i - 1) % nodes.size]
  end

  original_order = nodes.dup

  rounds.times do
    original_order.each do |node|
      moves = node.value % (nodes.size - 1)
      next if moves == 0

      # Remove node from current position
      node.prev.next = node.next
      node.next.prev = node.prev

      # Find new position
      current = node
      moves.abs.times { current = moves > 0 ? current.next : current.prev }

      # Insert node at new position
      if moves > 0
        node.next = current.next
        node.prev = current
        current.next.prev = node
        current.next = node
      else
        node.prev = current.prev
        node.next = current
        current.prev.next = node
        current.prev = node
      end
    end
  end

  nodes
end

def solve(numbers, decryption_key = 1, rounds = 1)
  numbers = numbers.map { |n| n * decryption_key }
  mixed = mix(numbers, rounds)

  zero_node = mixed.find { |node| node.value == 0 }
  [1000, 2000, 3000].sum do |n|
    current = zero_node
    n.times { current = current.next }
    current.value
  end
end

numbers = File.readlines('input.txt').map(&:to_i)

puts "Part 1: #{solve(numbers)}"
puts "Part 2: #{solve(numbers, 811589153, 10)}"
