
score = 0
depth = 0
inGarbage = false
cancelNext = false

file = open("input.txt", "r")

for line in eachline(file)
    for ch in line
        if cancelNext
            global cancelNext = false
            continue
        end

        if inGarbage
            if ch == '!'
                global cancelNext = true
            elseif ch == '>'
                global inGarbage = false
            end
        else
            if ch == '{'
                global depth += 1
            elseif ch == '}'
                global score += depth
                global depth -= 1
            elseif ch == '<'
                global inGarbage = true
            end
        end
    end
end

println(score)
