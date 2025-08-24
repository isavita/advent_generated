using MD5

struct Point
    x::Int
    y::Int
    path::String
end

function read_passcode(filename)
    open(filename) do file
        readline(file)
    end
end

function find_shortest_path(passcode)
    queue = [Point(0, 0, "")]
    while !isempty(queue)
        point = popfirst!(queue)
        if point.x == 3 && point.y == 3
            return point.path
        end
        for dir in get_open_doors(passcode, point.path)
            next_x = point.x
            next_y = point.y
            if dir == 'U'
                next_y -= 1
            elseif dir == 'D'
                next_y += 1
            elseif dir == 'L'
                next_x -= 1
            elseif dir == 'R'
                next_x += 1
            end
            if 0 <= next_x < 4 && 0 <= next_y < 4
                next_point = Point(next_x, next_y, point.path * dir)
                push!(queue, next_point)
            end
        end
    end
    return "No path found"
end

function get_open_doors(passcode, path)
    hash = bytes2hex(md5(passcode * path))
    doors = []
    if 'b' <= hash[1] <= 'f'
        push!(doors, 'U')
    end
    if 'b' <= hash[2] <= 'f'
        push!(doors, 'D')
    end
    if 'b' <= hash[3] <= 'f'
        push!(doors, 'L')
    end
    if 'b' <= hash[4] <= 'f'
        push!(doors, 'R')
    end
    return doors
end

passcode = read_passcode("input.txt")
path = find_shortest_path(passcode)
println(path)