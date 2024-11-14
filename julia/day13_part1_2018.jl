
using Printf

mutable struct Cart
    x::Int
    y::Int
    direction::Char
    turn_state::Int
end

function parse_input(filename)
    tracks = Vector{String}()
    carts = Vector{Cart}()

    open(filename, "r") do file
        for (y, line) in enumerate(eachline(file))
            push!(tracks, line)
            for (x, char) in enumerate(line)
                if char in ['<', '>', '^', 'v']
                    push!(carts, Cart(x-1, y-1, char, 0))
                end
            end
        end
    end

    return tracks, carts
end

function move_cart!(cart, tracks)
    # Update cart position based on direction
    if cart.direction == '^'
        cart.y -= 1
    elseif cart.direction == 'v'
        cart.y += 1
    elseif cart.direction == '<'
        cart.x -= 1
    elseif cart.direction == '>'
        cart.x += 1
    end

    # Handle track interactions
    current_track = tracks[cart.y+1][cart.x+1]
    
    if current_track == '/'
        cart.direction = Dict('^' => '>', 'v' => '<', '<' => 'v', '>' => '^')[cart.direction]
    elseif current_track == '\\'
        cart.direction = Dict('^' => '<', 'v' => '>', '<' => '^', '>' => 'v')[cart.direction]
    elseif current_track == '+'
        if cart.turn_state == 0  # Turn left
            cart.direction = Dict('^' => '<', 'v' => '>', '<' => 'v', '>' => '^')[cart.direction]
        elseif cart.turn_state == 2  # Turn right
            cart.direction = Dict('^' => '>', 'v' => '<', '<' => '^', '>' => 'v')[cart.direction]
        end
        cart.turn_state = (cart.turn_state + 1) % 3
    end
end

function find_first_collision(tracks, carts)
    while true
        # Sort carts by y, then x
        sort!(carts, by = cart -> (cart.y, cart.x))

        for (i, cart) in enumerate(carts)
            move_cart!(cart, tracks)

            # Check for collisions
            for j in 1:length(carts)
                if i != j && cart.x == carts[j].x && cart.y == carts[j].y
                    return cart.x, cart.y
                end
            end
        end
    end
end

function main()
    tracks, carts = parse_input("input.txt")
    collision_x, collision_y = find_first_collision(tracks, carts)
    @printf("First collision at: %d,%d\n", collision_x, collision_y)
end

main()
