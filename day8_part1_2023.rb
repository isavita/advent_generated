file = File.read("input.txt")
input = file.chomp

re = /[A-Z]{3}/

lines = input.split("\n")

desert_map = {}

lines[2..-1].each do |line|
    next if line.empty?

    matches = line.scan(re)
    desert_map[matches[0]] = { left: matches[1], right: matches[2] }
end

current = "AAA"
steps = 0

ElemToMatch = "ZZZ"

until current == ElemToMatch
    lines[0].strip.each_char do |direction|
        if direction == 'R'
            current = desert_map[current][:right]
        elsif direction == 'L'
            current = desert_map[current][:left]
        end
        steps += 1

        break if current == ElemToMatch
    end
end

puts steps