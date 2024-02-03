input = File.readlines('input.txt').map(&:chomp)

orbits = {}
input.each do |line|
  center, orbiter = line.split(')')
  orbits[orbiter] = center
end

def count_orbits(orbits, obj)
  return 0 unless orbits[obj]

  1 + count_orbits(orbits, orbits[obj])
end

total_orbits = orbits.keys.sum { |obj| count_orbits(orbits, obj) }

puts total_orbits

def path_to_com(orbits, obj)
  path = []
  while orbits[obj]
    path << orbits[obj]
    obj = orbits[obj]
  end
  path
end

you_path = path_to_com(orbits, 'YOU')
san_path = path_to_com(orbits, 'SAN')

common_obj = (you_path & san_path).first

you_transfers = you_path.index(common_obj)
san_transfers = san_path.index(common_obj)

puts you_transfers + san_transfers