
file = File.open("input.txt")
regex = /Game (\d+): (.+)/
cube_regex = /(\d+) (red|green|blue)/
total_power = 0

file.each_line do |line|
  matches = line.match(regex)

  if matches
    rounds = matches[2].split(";")
    max_red, max_green, max_blue = 0, 0, 0

    rounds.each do |round|
      cubes = round.scan(cube_regex)
      red, green, blue = 0, 0, 0

      cubes.each do |cube|
        count = cube[0].to_i
        case cube[1]
        when "red"
          red += count
        when "green"
          green += count
        when "blue"
          blue += count
        end
      end

      max_red = red if red > max_red
      max_green = green if green > max_green
      max_blue = blue if blue > max_blue
    end

    power = max_red * max_green * max_blue
    total_power += power
  end
end

puts total_power
