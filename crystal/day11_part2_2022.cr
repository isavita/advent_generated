
class Monkey
  property items : Array(Int64)
  property operation : Proc(Int64, Int64)
  property test_divisible : Int64
  property true_target : Int32
  property false_target : Int32
  property inspections : Int64

  def initialize(@items, @operation, @test_divisible, @true_target, @false_target)
    @inspections = 0_i64
  end
end

def solve(monkeys, rounds, worry_reducer)
  monkeys.each do |monkey|
    monkey.inspections = 0_i64
  end

  rounds.times do
    monkeys.each do |monkey|
      while !monkey.items.empty?
        item = monkey.items.shift
        monkey.inspections += 1

        # Apply operation and reduce worry
        worry_level = monkey.operation.call(item)
        worry_level = worry_reducer.call(worry_level)

        # Test and throw
        if worry_level % monkey.test_divisible == 0
          monkeys[monkey.true_target].items << worry_level
        else
          monkeys[monkey.false_target].items << worry_level
        end
      end
    end
  end

  # Return monkey business
  monkeys.map(&.inspections).sort.last(2).product
end

def parse_monkeys(input)
  monkeys = [] of Monkey
  input.split("\n\n").each do |monkey_data|
    lines = monkey_data.split("\n")
    
    # Parse starting items
    items = lines[1].split(":")[1].split(",").map(&.strip.to_i64)
    
    # Parse operation
    op_parts = lines[2].split("=")[1].strip.split(" ")
    operation = ->(old : Int64) do
      right = op_parts[2] == "old" ? old : op_parts[2].to_i64
      op_parts[1] == "*" ? old * right : old + right
    end
    
    # Parse test
    test_divisible = lines[3].split("by")[1].strip.to_i64
    true_target = lines[4].split("monkey")[1].strip.to_i
    false_target = lines[5].split("monkey")[1].strip.to_i

    monkeys << Monkey.new(items, operation, test_divisible, true_target, false_target)
  end
  monkeys
end

# Read input from file
input = File.read("input.txt")

# Part 1: Divide worry by 3
part1_monkeys = parse_monkeys(input)
part1_result = solve(part1_monkeys, 20, ->(x : Int64) { x // 3 })
puts "Part 1: #{part1_result}"

# Part 2: Manage worry levels
part2_monkeys = parse_monkeys(input)
mod_product = part2_monkeys.map(&.test_divisible).product
part2_result = solve(part2_monkeys, 10000, ->(x : Int64) { x % mod_product })
puts "Part 2: #{part2_result}"
