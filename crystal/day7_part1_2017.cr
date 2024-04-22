File.open("input.txt", "r") do |file|
  lines = file.each_line.map(&.chomp)

  holder_map = {} of String => Bool
  held_map = {} of String => Bool

  regex = /[a-z]+/

  lines.each do |line|
    names = line.scan(regex).map(&.to_s)
    holder = names[0]
    holder_map[holder] = true

    if names.size > 1
      names[1..].each do |name|
        held_map[name] = true
      end
    end
  end

  holder_map.each_key do |holder|
    if !held_map.has_key?(holder)
      puts holder
      exit
    end
  end
end