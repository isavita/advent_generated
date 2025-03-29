
using Printf

mutable struct Coord
    x::Int
    y::Int
    z::Int
end

mutable struct Brick
    id::Int # Added ID for easier tracking if needed
    mini::Coord
    maxi::Coord
    basedOn::Vector{Int} # Indices of bricks this one rests on
    support::Vector{Int} # Indices of bricks this one supports
end

# Constructor for easier initialization
Brick(id::Int, mini::Coord, maxi::Coord) = Brick(id, mini, maxi, Int[], Int[])

function parseInput(filename::String)
    bricks = Brick[]
    lines = readlines(filename)
    for (i, line) in enumerate(lines)
        parts = split(line, '~')
        p1_str = split(parts[1], ',')
        p2_str = split(parts[2], ',')
        
        x1, y1, z1 = parse.(Int, p1_str)
        x2, y2, z2 = parse.(Int, p2_str)

        # Ensure mini has the smaller coordinates and maxi has the larger ones
        mini = Coord(min(x1, x2), min(y1, y2), min(z1, z2))
        maxi = Coord(max(x1, x2), max(y1, y2), max(z1, z2))
        
        push!(bricks, Brick(i, mini, maxi))
    end
    return bricks
end

function overlaps(b1::Brick, b2::Brick)
    return max(b1.mini.x, b2.mini.x) <= min(b1.maxi.x, b2.maxi.x) &&
           max(b1.mini.y, b2.mini.y) <= min(b1.maxi.y, b2.maxi.y)
end

function settle!(bricks::Vector{Brick})
    # Sort bricks primarily by minimum Z coordinate for settling order
    sort!(bricks, by = b -> b.mini.z)

    n = length(bricks)
    for i in 1:n
        brick = bricks[i]
        supportZ = 0
        basedBricks_indices = Int[]

        # Check bricks below (already settled)
        for j in 1:i-1
            below_brick = bricks[j]
            if overlaps(brick, below_brick)
                 if below_brick.maxi.z > supportZ
                    supportZ = below_brick.maxi.z
                    basedBricks_indices = [j] # Start new list
                 elseif below_brick.maxi.z == supportZ
                    push!(basedBricks_indices, j) # Add to list
                 end
            end
        end

        # Update basedOn for the current brick (using indices relative to sorted array)
        brick.basedOn = basedBricks_indices

        # Update support for the bricks below
        for base_idx in basedBricks_indices
            push!(bricks[base_idx].support, i)
        end

        # Update Z position
        deltaZ = brick.maxi.z - brick.mini.z
        brick.mini.z = supportZ + 1
        brick.maxi.z = brick.mini.z + deltaZ
    end
    
    # Re-sort by the new minimum Z coordinate after settling, might be useful
    # but not strictly necessary for the next step if using indices correctly.
    # sort!(bricks, by = b -> b.mini.z) # Keep original indices correct
end


function solve(filename::String)
    bricks = parseInput(filename)
    settle!(bricks)

    n = length(bricks)
    total_fall_count = 0

    for i in 1:n # Consider disintegrating brick i
        
        falling_bricks_indices = Set{Int}() # Indices of bricks that would fall
        queue = Int[] # Queue for BFS (indices)

        # Start BFS with bricks directly supported by brick i, if i is their *only* support
        for supported_idx in bricks[i].support
            supported_brick = bricks[supported_idx]
            if length(supported_brick.basedOn) == 1
                 # Since bricks[i] was the only support, it will fall
                 # Add to set and queue only if not already processed
                 if !(supported_idx in falling_bricks_indices)
                     push!(falling_bricks_indices, supported_idx)
                     push!(queue, supported_idx)
                 end
            end
        end

        # Process the queue using BFS
        queue_idx = 1
        while queue_idx <= length(queue)
            current_falling_idx = queue[queue_idx]
            queue_idx += 1

            current_falling_brick = bricks[current_falling_idx]

            # Check bricks supported by the current falling brick
            for check_support_idx in current_falling_brick.support
                 # If already marked as falling, skip
                 if check_support_idx in falling_bricks_indices
                     continue
                 end

                 check_brick = bricks[check_support_idx]
                 
                 # Check if all supports of 'check_brick' are either disintegrated (i) or falling
                 all_supports_gone = true
                 for base_idx in check_brick.basedOn
                    # If a support is not brick 'i' and not in the set of falling bricks,
                    # then 'check_brick' will not fall (yet).
                     if base_idx != i && !(base_idx in falling_bricks_indices)
                         all_supports_gone = false
                         break
                     end
                 end

                 if all_supports_gone
                     # This brick will also fall
                     if !(check_support_idx in falling_bricks_indices)
                         push!(falling_bricks_indices, check_support_idx)
                         push!(queue, check_support_idx)
                     end
                 end
            end
        end
        
        total_fall_count += length(falling_bricks_indices)
    end

    return total_fall_count
end

function main()
    answer = solve("input.txt")
    println(answer)
end

main()
