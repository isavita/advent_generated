
module AuntSue
  def self.solve(input_file : String) : Int32
    target = {
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

    File.each_line(input_file) do |line|
      match = line.match(/^Sue (\d+): (.*)$/)
      next unless match

      sue_num = match[1].to_i
      attributes = match[2].split(", ").map { |attr| attr.split(": ") }.to_h

      if attributes.all? { |k, v| target[k] == v.to_i }
        return sue_num
      end
    end

    -1 # No matching Sue found
  end
end

puts AuntSue.solve("input.txt")
