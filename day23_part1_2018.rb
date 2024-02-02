nanobots = File.readlines('input.txt').map(&:chomp)

nanobots.map! { |nanobot| nanobot.scan(/-?\d+/).map(&:to_i) }

strongest_nanobot = nanobots.max_by { |nanobot| nanobot[3] }

in_range = nanobots.count { |nanobot| (nanobot[0] - strongest_nanobot[0]).abs + (nanobot[1] - strongest_nanobot[1]).abs + (nanobot[2] - strongest_nanobot[2]).abs <= strongest_nanobot[3] }

puts in_range