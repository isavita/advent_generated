
function parse_data(data)
    garden = Dict{Complex{Int}, Bool}()
    start = -1 + 0im
    for (y, line) in enumerate(data)
        for (x, c) in enumerate(line)
            if c != '#'
                garden[x-1 + (y-1)im] = true
            end
            if c == 'S'
                start = (x-1) + (y-1)im
            end
        end
    end
    
    @assert start != -1+0im "No start found!"
    return garden, start
end

function complex_mod(num::Complex{Int}, mod::Int)
    return Complex((real(num) + 10*mod) % mod, (imag(num) + 10*mod) % mod)
end

function calculate_num_ends(garden, start, num_iterations, max_size)
    queue = Set{Complex{Int}}([start])
    done = Int[]

    for i in 0:3*max_size-1
        if (i % max_size) == (max_size-1)÷2
            push!(done, length(queue))
        end
        
        if length(done) == 3
            break
        end

        new_queue = Set{Complex{Int}}()

        for dir in [1, -1, 1im, -1im]
            for point in queue
                mod_point = complex_mod(point + dir, max_size)
                if haskey(garden, mod_point)
                    push!(new_queue, point + dir)
                end
            end
        end
        queue = new_queue
    end

    function quadratic_function(n, a, b, c)
        return a + n*(b-a+((n-1)*(c-2*b+a)÷2))
    end

    return quadratic_function(num_iterations÷max_size, done[1], done[2], done[3])
end

function main()
    garden_input = readlines("input.txt")
    garden, start = parse_data(garden_input)
    max_size = length(garden_input)

    sum = calculate_num_ends(garden, start, 26501365, max_size)
    println(sum)
end

main()
