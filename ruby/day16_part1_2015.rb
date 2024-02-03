
require 'set'

mfcsam = {
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1
}

aunts = File.readlines('input.txt').map(&:chomp)

aunts.each_with_index do |aunt, index|
  properties = aunt.scan(/(\w+): (\d+)/).to_h
  next unless properties.all? { |key, value| mfcsam[key.to_sym] == value.to_i }

  puts index + 1
  break
end
