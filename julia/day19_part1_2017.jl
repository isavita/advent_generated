function read_input(filename)
    data = readlines(filename)
    return data
end

function follow_path(data)
    x, y = 1, findfirst(isequal('|'), data[1])[1]
    dx, dy = 1, 0
    letters = []
    
    while true
        x += dx
        y += dy
        
        if x < 1 || x > length(data) || y < 1 || y > length(data[x])
            break
        end
        
        if data[x][y] == ' '
            break
        elseif isletter(data[x][y])
            push!(letters, data[x][y])
        elseif data[x][y] == '+'
            if dx != 0
                dx = 0
                if y > 1 && y <= length(data[x]) && data[x][y-1] != ' '
                    dy = -1
                else
                    dy = 1
                end
            else
                dy = 0
                if x > 1 && x <= length(data) && data[x-1][y] != ' '
                    dx = -1
                else
                    dx = 1
                end
            end
        end
    end
    
    return join(letters)
end

data = read_input("input.txt")
result = follow_path(data)
println(result)