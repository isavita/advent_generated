require "file"

def aunt_sue(input)
  target_sue = {
    "children"    => 3,
    "cats"        => 7,
    "samoyeds"    => 2,
    "pomeranians" => 3,
    "akitas"      => 0,
    "vizslas"     => 0,
    "goldfish"    => 5,
    "trees"       => 3,
    "cars"        => 2,
    "perfumes"    => 1,
  }

  input.split("\n").each do |line|
    match = line.match(/Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)/)
    next unless match
    sue_num, thing1, amount1, thing2, amount2, thing3, amount3 = match[1].to_i, match[2], match[3].to_i, match[4], match[5].to_i, match[6], match[7].to_i
    readings_map = {
      thing1 => amount1,
      thing2 => amount2,
      thing3 => amount3,
    }
    all_rules_matched = true

    ["cats", "trees"].each do |check|
      if readings_map[check]? && readings_map[check] <= target_sue[check]
        all_rules_matched = false
      end
      readings_map.delete(check)
    end

    ["pomeranians", "goldfish"].each do |check|
      if readings_map[check]? && readings_map[check] >= target_sue[check]
        all_rules_matched = false
      end
      readings_map.delete(check)
    end

    readings_map.each do |thing, amount|
      unless amount == target_sue[thing]?
        all_rules_matched = false
      end
    end

    return sue_num if all_rules_matched
  end
  raise "expect return from loop"
end

input = File.read("input.txt")
puts aunt_sue(input)