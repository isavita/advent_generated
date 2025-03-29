
using Printf

mutable struct Cart
    position::Tuple{Int, Int}
    direction::Char
    next_turn::Int # 0: left, 1: straight, 2: right
end

const MOVES = Dict('^' => (0, -1), 'v' => (0, 1), '<' => (-1, 0), '>' => (1, 0))
const TURN_SLASH = Dict('^' => '>', 'v' => '<', '<' => 'v', '>' => '^') # /
const TURN_BACKSLASH = Dict('^' => '<', 'v' => '>', '<' => '^', '>' => 'v') # \
const TURN_LEFT = Dict('^' => '<', 'v' => '>', '<' => 'v', '>' => '^')
const TURN_RIGHT = Dict('^' => '>', 'v' => '<', '<' => '^', '>' => 'v')

function parse_input(file_path::String)
    lines = readlines(file_path)
    tracks = Dict{Tuple{Int, Int}, Char}()
    carts = Vector{Cart}()
    for (y, line) in enumerate(lines)
        for (x, char) in enumerate(line)
            pos = (x - 1, y - 1) # Use 0-based indexing for coordinates
            if char in "^v<>"
                push!(carts, Cart(pos, char, 0))
                tracks[pos] = (char in "v^") ? '|' : '-'
            elseif char in "|-+/\\"
                tracks[pos] = char
            end
        end
    end
    return tracks, carts
end

function move_cart!(cart::Cart, tracks::Dict{Tuple{Int, Int}, Char})
    dx, dy = MOVES[cart.direction]
    x, y = cart.position
    new_x, new_y = x + dx, y + dy
    new_pos = (new_x, new_y)

    track = tracks[new_pos]
    new_dir = cart.direction

    if track == '/'
        new_dir = TURN_SLASH[cart.direction]
    elseif track == '\\'
        new_dir = TURN_BACKSLASH[cart.direction]
    elseif track == '+'
        if cart.next_turn == 0 # left
            new_dir = TURN_LEFT[cart.direction]
        elseif cart.next_turn == 2 # right
            new_dir = TURN_RIGHT[cart.direction]
        end
        #elseif cart.next_turn == 1 # straight - direction doesn't change
        cart.next_turn = (cart.next_turn + 1) % 3
    end

    cart.position = new_pos
    cart.direction = new_dir
end

function simulate(tracks::Dict{Tuple{Int, Int}, Char}, initial_carts::Vector{Cart})
    carts = copy(initial_carts)
    occupied = Set{Tuple{Int, Int}}(cart.position for cart in carts)

    while length(carts) > 1
        sort!(carts, by = c -> (c.position[2], c.position[1]))
        crashed_indices = Set{Int}()
        new_occupied = Set{Tuple{Int, Int}}()

        for i in 1:length(carts)
            if i in crashed_indices
                continue
            end

            cart = carts[i]
            current_pos = cart.position
            delete!(occupied, current_pos) # Remove old position

            move_cart!(cart, tracks)
            new_pos = cart.position

            if new_pos in occupied || new_pos in new_occupied # Collision check
                 # Find the other cart(s) involved
                 collided_with_new = false
                 for k in 1:length(carts)
                     if i != k && !(k in crashed_indices) && carts[k].position == new_pos
                         push!(crashed_indices, k)
                         collided_with_new = true
                     end
                 end

                 if new_pos in new_occupied # Collision with a cart moved earlier in this tick
                     delete!(new_occupied, new_pos) # Remove the position from newly occupied
                     push!(crashed_indices, i)
                 elseif new_pos in occupied # Collision with a cart not yet moved this tick
                     delete!(occupied, new_pos) # Remove the position from occupied
                     push!(crashed_indices, i)
                 end


            else
                push!(new_occupied, new_pos) # Add new position if no crash
            end
        end

        # Rebuild carts list excluding crashed ones and update occupied set
        survivors = Vector{Cart}()
        occupied = Set{Tuple{Int, Int}}()
        for i in 1:length(carts)
            if !(i in crashed_indices)
                push!(survivors, carts[i])
                push!(occupied, carts[i].position)
            end
        end
        carts = survivors

    end
    return carts[1].position
end

function main()
    tracks, carts = parse_input("input.txt")
    last_cart_position = simulate(tracks, carts)
    println("$(last_cart_position[1]),$(last_cart_position[2])")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
