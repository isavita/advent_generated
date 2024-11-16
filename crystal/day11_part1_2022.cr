
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

def parse_monkeys(input : String) : Array(Monkey)
  monkeys = [] of Monkey
  input.split("\n\n").each do |monkey_data|
    lines = monkey_data.split("\n")
    
    # Parse starting items
    items = lines[1].split(":")[1].split(",").map(&.strip.to_i64)
    
    # Parse operation
    op_parts = lines[2].split("=")[1].strip.split(" ")
    operation = ->(old : Int64) do
      right = op_parts[2] == "old" ? old : op_parts[2].to_i64
      case op_parts[1]
      when "*" then old * right
      when "+" then old + right
      else
        raise "Unknown operation"
      end
    end
    
    # Parse test details
    test_divisible = lines[3].split("by")[1].strip.to_i64
    true_target = lines[4].split("monkey")[1].strip.to_i32
    false_target = lines[5].split("monkey")[1].strip.to_i32
    
    monkeys << Monkey.new(items, operation, test_divisible, true_target, false_target)
  end
  monkeys
end

def simulate_rounds(monkeys : Array(Monkey), rounds : Int32)
  rounds.times do
    monkeys.each_with_index do |monkey, _|
      while !monkey.items.empty?
        item = monkey.items.shift
        monkey.inspections += 1
        
        # Inspect and reduce worry
        worry_level = monkey.operation.call(item) // 3
        
        # Test and throw
        if worry_level % monkey.test_divisible == 0
          monkeys[monkey.true_target].items << worry_level
        else
          monkeys[monkey.false_target].items << worry_level
        end
      end
    end
  end
end

# Read input from file
input = File.read("input.txt")

# Parse monkeys and simulate rounds
monkeys = parse_monkeys(input)
simulate_rounds(monkeys, 20)

# Calculate monkey business
inspections = monkeys.map(&.inspections).sort.reverse
puts inspections[0] * inspections[1]
