
regex = /Game (\d+): (.+)/
cube_regex = /(\d+) (red|green|blue)/
total_sum = 0

File.open("input.txt", "r") do |file|
  file.each_line do |line|
    matches = regex.match(line)

    if matches
      game_id = matches[1].to_i
      rounds = matches[2].split(";")
      is_valid = true

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

          if red > 12 || green > 13 || blue > 14
            is_valid = false
            break
          end
        end

        break unless is_valid
      end

      total_sum += game_id if is_valid
    end
  end
end

puts total_sum
