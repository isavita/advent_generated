
class Rule
  attr_reader :category, :operator, :num, :workflow_name

  def initialize(category: nil, operator: nil, num: nil, workflow_name:)
    @category = category
    @operator = operator
    @num = num
    @workflow_name = workflow_name
  end
end

class Solver
  def initialize(input_file)
    @input = File.read(input_file).strip.split("\n")
    @workflows = parse_workflows
    @start_workflow = 'in'
    @min_rating = 1
    @max_rating = 4000
  end

  def solve
    initial_interval = {
      'x' => [1, 4000],
      'm' => [1, 4000],
      'a' => [1, 4000],
      's' => [1, 4000]
    }
    apply_workflow_interval(initial_interval, @start_workflow)
  end

  private

  def parse_workflows
    workflows = {}
    @input.take_while { |line| !line.empty? }.each do |line|
      name, rules_str = line.split('{')
      rules_str = rules_str.chop
      rules = rules_str.split(',').map do |rule_str|
        if rule_str.include?(':')
          category = rule_str[0]
          operator = rule_str[1]
          num, workflow_name = rule_str[2..-1].split(':')
          Rule.new(category: category, operator: operator, num: num.to_i, workflow_name: workflow_name)
        else
          Rule.new(workflow_name: rule_str)
        end
      end
      workflows[name] = rules
    end
    workflows
  end

  def apply_workflow_interval(part_interval, workflow_name)
    return 0 if workflow_name == 'R'
    
    return calculate_combinations(part_interval) if workflow_name == 'A'

    total_combinations = 0
    @workflows[workflow_name].each do |rule|
      if rule.operator.nil?
        total_combinations += apply_workflow_interval(part_interval, rule.workflow_name)
        break
      end

      interval = part_interval[rule.category]
      valid_interval, invalid_interval = split_interval(interval, rule)

      if valid_interval
        new_part_interval = part_interval.dup
        new_part_interval[rule.category] = valid_interval
        total_combinations += apply_workflow_interval(new_part_interval, rule.workflow_name)
      end

      break unless invalid_interval
      part_interval[rule.category] = invalid_interval
    end

    total_combinations
  end

  def split_interval(interval, rule)
    start, end_val = interval
    case rule.operator
    when '>'
      return nil if start > rule.num
      valid_interval = rule.num + 1 <= end_val ? [rule.num + 1, end_val] : nil
      invalid_interval = start <= rule.num ? [start, rule.num] : nil
    when '<'
      return nil if end_val < rule.num
      valid_interval = start <= rule.num - 1 ? [start, rule.num - 1] : nil
      invalid_interval = rule.num <= end_val ? [rule.num, end_val] : nil
    end
    [valid_interval, invalid_interval]
  end

  def calculate_combinations(part_interval)
    part_interval.values.map { |interval| interval[1] - interval[0] + 1 }.reduce(:*)
  end
end

# Main execution
solver = Solver.new('input.txt')
puts solver.solve
