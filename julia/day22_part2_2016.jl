
using DataStructures

struct Point
    x::Int
    y::Int
end

struct Node
    used::Int
    avail::Int
end

const NEIGHBORS4 = [Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)]
const RE_PATTERN = r"-x(\d+)-y(\d+)"

function parse_input(filename::String)
    nodes = Dict{Point, Node}()
    lines = readlines(filename)
    for line in lines[3:end]  # Skip header lines
        fields = split(line)
        m = match(RE_PATTERN, fields[1])
        if m !== nothing
            x = parse(Int, m.captures[1])
            y = parse(Int, m.captures[2])
            p = Point(x, y)
            used = parse(Int, fields[3][1:end-1]) # Remove 'T'
            avail = parse(Int, fields[4][1:end-1]) # Remove 'T'
            nodes[p] = Node(used, avail)
        end
    end
    return nodes
end

function get_dims(nodes::Dict{Point, Node})
    w, h = 0, 0
    for p in keys(nodes)
        w = max(w, p.x)
        h = max(h, p.y)
    end
    return w, h
end

function find_hole(nodes::Dict{Point, Node})
    for (p, n) in nodes
        if n.used == 0
            return p
        end
    end
    error("no hole found")
end

# BFS to find shortest path for the hole
function moves(nodes::Dict{Point, Node}, w::Int, h::Int, goal_data_pos::Point, start_hole::Point, target_hole::Point)
    if start_hole == target_hole
        return 0
    end

    q = Deque{Point}()
    push!(q, start_hole)
    visited = Dict{Point, Int}(start_hole => 0)

    while !isempty(q)
        curr_pos = popfirst!(q)
        curr_depth = visited[curr_pos]

        if curr_pos == target_hole
            return curr_depth
        end

        next_depth = curr_depth + 1

        for n in NEIGHBORS4
            next_pos = Point(curr_pos.x + n.x, curr_pos.y + n.y)

            # Check bounds
            if next_pos.x < 0 || next_pos.y < 0 || next_pos.x > w || next_pos.y > h
                continue
            end

            # Check if it's the goal data location or an immovable wall (high usage)
            # The hole cannot move into the spot currently occupied by the goal data
            # or into a node that cannot hold the data from the hole (which is 0 used, so anything works except walls)
             node = nodes[next_pos]
             if next_pos == goal_data_pos || node.used > 400 # Assuming 'walls' are nodes too large to ever swap with
                  continue
             end

            # Check if already visited with a shorter or equal path
            if haskey(visited, next_pos) && visited[next_pos] <= next_depth
                 continue
            end

            visited[next_pos] = next_depth
            push!(q, next_pos)
        end
    end

    error("no possible path for hole from $start_hole to $target_hole")
end


function solve()
    nodes = parse_input("input.txt")
    w, h = get_dims(nodes)

    goal_data_pos = Point(w, 0)
    hole_pos = find_hole(nodes)
    total_moves = 0

    while goal_data_pos != Point(0, 0)
        # Target position for the hole is adjacent (left) to the goal data
        hole_target_pos = Point(goal_data_pos.x - 1, goal_data_pos.y)

        # 1. Calculate moves needed to bring the hole to the target position
        m = moves(nodes, w, h, goal_data_pos, hole_pos, hole_target_pos)
        total_moves += m

        # Update hole position (conceptually, it moved)
        hole_pos = hole_target_pos

        # 2. Add 1 move for swapping the goal data into the now adjacent hole
        total_moves += 1

        # 3. Update positions: the old goal location is now the hole,
        #    and the old hole location (where the data moved into) is the new goal data position.
        new_hole_pos = goal_data_pos
        goal_data_pos = hole_pos # The data is now where the hole was (adjacent left)
        hole_pos = new_hole_pos # The hole is now where the data was
    end

    println(total_moves)
end

function main()
    solve()
end

main()
