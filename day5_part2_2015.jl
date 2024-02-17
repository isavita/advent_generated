
input = read("input.txt", String)
nice = 0

function passesRule1(line)
    for i in 1:length(line)-2
        toMatch = line[i:i+1]
        for j in i+2:length(line)-1
            if line[j:j+1] == toMatch
                return true
            end
        end
    end
    return false
end

for line in split(input, "\n")
    rule1 = passesRule1(line)
    rule2 = false
    for i in 1:length(line)-2
        if line[i] == line[i+2]
            rule2 = true
            break
        end
    end
    if rule1 && rule2
        global nice += 1  # Using global to access the outer 'nice' variable
    end
end

println(nice)
