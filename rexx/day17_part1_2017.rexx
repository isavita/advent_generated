input = linein("input.txt")
steps = strip(input)

buffer = '0'
currentPos = 1

do i = 1 to 2017
    currentPos = (currentPos + steps) % i + 1
    buffer = substr(buffer, 1, currentPos) || i || substr(buffer, currentPos + 1)
    currentPos = currentPos + 1
end

parse var buffer before 2017 after
say after