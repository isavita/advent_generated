
gift_data = {
  "children" => 3,
  "cats" => 7, # greater than
  "samoyeds" => 2,
  "pomeranians" => 3, # fewer than
  "akitas" => 0,
  "vizslas" => 0,
  "goldfish" => 5, # fewer than
  "trees" => 3, # greater than
  "cars" => 2,
  "perfumes" => 1
}

def find_sue(sues, gift_data, part_two = false)
  sues.each do |sue, properties|
    match = properties.all? do |prop, value|
      if part_two
        case prop
        when "cats", "trees"
          value > gift_data[prop]
        when "pomeranians", "goldfish"
          value < gift_data[prop]
        else
          value == gift_data[prop]
        end
      else
        gift_data[prop] == value
      end
    end
    return sue if match
  end
end

sues = {}
File.readlines("input.txt").each do |line|
  sue, properties = line.strip.match(/^Sue (\d+): (.*)$/).captures
  properties = properties.split(", ").map { |prop| prop.split(": ") }.to_h
  properties.transform_values!(&:to_i)
  sues[sue] = properties
end

puts find_sue(sues, gift_data)
puts find_sue(sues, gift_data, true)
