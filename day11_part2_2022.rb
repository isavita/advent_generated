
class Monkey
  attr_accessor :items, :operation, :div, :next, :inspections

  def initialize
    @items = []
    @next = [0, 0]
    @inspections = 0
  end
end

def parse(s)
  m = Monkey.new
  lines = s.split("\n")
  lines[1].split(": ")[1].split(", ").each { |item| m.items << item.to_i }
  f = lines[2].split("= ")[1].split(" ")
  case f[1]
  when "+"
    m.operation = f[2] == "old" ? ->(old) { old + old } : ->(old) { old + f[2].to_i }
  when "*"
    m.operation = f[2] == "old" ? ->(old) { old * old } : ->(old) { old * f[2].to_i }
  end
  m.div = lines[3].scan(/\d+/).first.to_i
  m.next[0] = lines[4].scan(/\d+/).first.to_i
  m.next[1] = lines[5].scan(/\d+/).first.to_i
  m
end

def monkey_business(monkeys, rounds, worry)
  div = 1
  monkeys.each { |m| div *= m.div }

  rounds.times do
    monkeys.each do |m|
      while !m.items.empty?
        m.inspections += 1
        item = m.operation.call(m.items[0])
        if worry
          item %= div
        else
          item /= 3
        end
        if item % m.div == 0
          monkeys[m.next[0]].items << item
        else
          monkeys[m.next[1]].items << item
        end
        m.items.shift
      end
    end
  end

  inspections = monkeys.map(&:inspections).sort.reverse
  inspections[0] * inspections[1]
end

monkeys = []
File.foreach("input.txt", "\n\n") do |m|
  monkeys << parse(m)
end

puts monkey_business(monkeys, 10000, true)
