File.open("input.txt", "r") do |file|
  coordinates = file.gets_to_end.split("\n").map { |line| line.split(", ").map(&.to_i) }
  max_x = coordinates.map(&.[0]).max
  max_y = coordinates.map(&.[1]).max

  areas = Array.new(coordinates.size, 0)
  infinite_areas = Set(Int32).new

  (0..max_x).each do |x|
    (0..max_y).each do |y|
      min_distance = Int32::MAX
      closest_coordinate = -1
      coordinates.each_with_index do |coord, i|
        distance = (coord[0] - x).abs + (coord[1] - y).abs
        if distance < min_distance
          min_distance = distance
          closest_coordinate = i
        elsif distance == min_distance
          closest_coordinate = -1
        end
      end
      if closest_coordinate >= 0
        areas[closest_coordinate] += 1
        if x == 0 || x == max_x || y == 0 || y == max_y
          infinite_areas.add(closest_coordinate)
        end
      end
    end
  end

  puts areas.reject { |area| infinite_areas.includes?(areas.index(area)) }.max
end