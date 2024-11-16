
def parse_game(line : String)
  game_id, sets = line.split(": ")
  game_id = game_id.split(" ")[1].to_i
  
  max_cubes = {
    "red" => 0,
    "green" => 0,
    "blue" => 0
  }
  
  sets.split("; ").each do |set|
    set.split(", ").each do |cube|
      count, color = cube.split(" ")
      max_cubes[color] = [max_cubes[color], count.to_i].max
    end
  end
  
  {game_id, max_cubes}
end

def part1(games)
  games.select do |game_id, max_cubes|
    max_cubes["red"] <= 12 &&
    max_cubes["green"] <= 13 &&
    max_cubes["blue"] <= 14
  end.sum { |game_id, _| game_id }
end

def part2(games)
  games.sum do |_, max_cubes|
    max_cubes["red"] * max_cubes["green"] * max_cubes["blue"]
  end
end

games = File.read_lines("input.txt").map { |line| parse_game(line) }

puts "Part 1: #{part1(games)}"
puts "Part 2: #{part2(games)}"
