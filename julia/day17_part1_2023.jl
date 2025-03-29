
using DataStructures

struct Coord
    x::Int
    y::Int
end

import Base: +, -, ==, isless
+(a::Coord, b::Coord) = Coord(a.x + b.x, a.y + b.y)
-(a::Coord, b::Coord) = Coord(a.x - b.x, a.y - b.y)
opposite(c::Coord) = Coord(-c.x, -c.y)
# isless for potential tie-breaking in PriorityQueue, though not strictly needed by A*
isless(a::Coord, b::Coord) = (a.x, a.y) < (b.x, b.y)

struct Grid
    data::Matrix{Int}
    height::Int
    width::Int
end

function Grid(lines::Vector{String})
    height = length(lines)
    width = length(lines[1])
    data = Matrix{Int}(undef, height, width)
    for r in 1:height
        for c in 1:width
            data[r, c] = parse(Int, lines[r][c])
        end
    end
    return Grid(data, height, width)
end

const DIRECTIONS = [Coord(0, -1), Coord(-1, 0), Coord(0, 1), Coord(1, 0)] # (dx, dy)

function neighbors4(grid::Grid, coord::Coord)
    neighbors = Coord[]
    for dir in DIRECTIONS
        neighbor = coord + dir
        if 1 <= neighbor.y <= grid.height && 1 <= neighbor.x <= grid.width
            push!(neighbors, neighbor)
        end
    end
    return neighbors
end

# State: (coordinate, direction_entered, consecutive_steps_in_direction)
const State = Tuple{Coord, Coord, Int}

function a_star_constrained(grid::Grid, start::Coord, goal::Coord, min_straight::Int, max_straight::Int)
    # Initial state: at start, direction (0,0), 0 steps taken
    start_state = (start, Coord(0, 0), 0)

    # Priority queue stores (priority, state)
    # Priority = cost_so_far + heuristic
    frontier = PriorityQueue{State, Int}()
    enqueue!(frontier, start_state, 0)

    # cost_so_far stores the minimum cost to reach a state
    cost_so_far = Dict{State, Int}()
    cost_so_far[start_state] = 0

    while !isempty(frontier)
        current_state, current_priority_no_heuristic = dequeue_pair!(frontier)
        current_coord, current_dir, current_steps = current_state

        # Cost used for comparison should be the actual cost, not including heuristic
        current_cost = cost_so_far[current_state]

        # Optimization: If we already found a shorter path to this state, skip
        # This check is implicitly handled by the cost comparison before enqueueing,
        # but can sometimes prune redundant states pulled later from the queue.
        # However, standard A* usually doesn't need this explicit check here if
        # the cost check before enqueueing is correct. Let's keep it simple.


        # Goal check: Must have moved at least min_straight steps to stop at the goal
        # (This detail is often required in such problems, although the Python code doesn't explicitly check it here)
        # Let's adhere strictly to the python code's logic for now which returns cost upon reaching goal coord.
        if current_coord == goal # && current_steps >= min_straight # <- Optional stricter goal condition
             # The python code returns the cost of the state that *lands* on the goal.
             # We need to find the minimum cost among *all states* ending at the goal coord.
             # Let's adjust the logic slightly: keep track of the minimum cost found *for the goal coordinate*.
             # No, the original logic is fine: A* finds the minimum cost path *to* the goal state.
             # Once a state with the goal coordinate is dequeued, it's guaranteed to be the minimum cost path
             # *to that specific state* (coord, dir, steps). Since costs are non-negative, the *first* time
             # we dequeue *any* state whose coordinate is the goal, it must be via an optimal path.
             # However, we must respect the min_straight rule for the *final* move.
             # Re-reading the Python: it just checks `current_coord == goal`. Let's stick to that.
             # The constraints are checked *before* adding a state to the queue.
             return current_cost
        end

        for neighbor in neighbors4(grid, current_coord)
            new_dir = neighbor - current_coord
            
            # Rule: Cannot immediately reverse direction (unless at start)
            if current_dir != Coord(0, 0) && new_dir == opposite(current_dir)
                continue
            end

            new_steps = (new_dir == current_dir) ? current_steps + 1 : 1

            # Rule: Cannot exceed max_straight steps in one direction
            if new_steps > max_straight
                continue
            end

            # Rule: Must move at least min_straight steps before turning
            # (This doesn't apply if we are continuing straight)
            # (This doesn't apply at the very start coord where current_dir is (0,0))
            if current_dir != Coord(0, 0) && new_dir != current_dir && current_steps < min_straight
                 continue
            end
            
            # Rule for Part 2: must move min_straight steps *to arrive* at the goal
            if neighbor == goal && new_steps < min_straight
                continue
            end


            neighbor_state = (neighbor, new_dir, new_steps)
            new_cost = current_cost + grid.data[neighbor.y, neighbor.x] # Julia uses 1-based [row, col] = [y, x]

            if !haskey(cost_so_far, neighbor_state) || new_cost < cost_so_far[neighbor_state]
                 cost_so_far[neighbor_state] = new_cost
                 # Heuristic: Manhattan distance
                 priority = new_cost + abs(neighbor.x - goal.x) + abs(neighbor.y - goal.y)
                 enqueue!(frontier, neighbor_state, priority)
            end
        end
    end

    return -1 # Goal not reachable
end


function solve(lines::Vector{String})
    grid = Grid(lines)
    start_coord = Coord(1, 1) # Julia is 1-based
    goal_coord = Coord(grid.width, grid.height) # Julia is 1-based
    
    # Use constraints from the Python code provided (Part 1 type constraints)
    # For Part 2 use min_straight=4, max_straight=10 and add goal check rule above.
    # Assuming part 1 based on the python code:
    min_s = 0 # Python code used 0
    max_s = 3 # Python code used 3
    
    # Re-adjust min_straight for clarity based on common AoC interpretation:
    # min_straight=1 means you must take at least 1 step. Python's 0 is slightly ambiguous
    # but implies you *can* turn immediately. Let's assume it means min 1 step to turn.
    # Wait, python code clearly uses min_straight=0 in the check: `current_num_straight >= min_straight`
    # If min_straight=0, this condition is *always* true. So the only turn constraint is implicitly
    # handled by not reversing and not exceeding max_straight. Let's stick to 0.
    
    # Rerun logic with min_straight=0:
    # `(current_steps >= 0 || new_dir == current_dir || current_coord == start_coord)` -> always true
    # So the actual constraints enforced by the Python code are:
    # 1. `new_steps <= max_straight` (3)
    # 2. `new_dir != opposite(current_dir)` (no reversing)
    
    # Let's implement exactly that behavior for consistency.
    # The goal check modification for Part 2 min_steps is also removed for now.

    # Re-writing a simpler A* only enforcing max_straight and no-reverse.
    # This matches the Python code's effective logic with min_straight=0.

    start_state = (start_coord, Coord(0, 0), 0)
    frontier = PriorityQueue{State, Int}()
    enqueue!(frontier, start_state, 0)
    cost_so_far = Dict{State, Int}()
    cost_so_far[start_state] = 0

    min_cost_to_goal = typemax(Int) # Keep track of the minimum cost found *for any state* at the goal coordinate

    while !isempty(frontier)
        current_state, _ = dequeue_pair!(frontier) # Priority not needed after dequeue
        current_coord, current_dir, current_steps = current_state
        current_cost = cost_so_far[current_state]
        
        # Optimization: If we've already found a path to the goal cheaper than the current path, prune.
        if current_cost >= min_cost_to_goal
            continue
        end

        if current_coord == goal_coord
            # Update the minimum cost found to reach the goal coordinate
             min_cost_to_goal = min(min_cost_to_goal, current_cost)
             # Continue searching, as another path might reach the goal with a different state (dir, steps) but lower cost
             continue # Important: Don't return immediately, explore other paths to goal
        end

        for neighbor in neighbors4(grid, current_coord)
            new_dir = neighbor - current_coord

            # Rule: Cannot immediately reverse direction (unless at start)
            if current_dir != Coord(0, 0) && new_dir == opposite(current_dir)
                continue
            end

            new_steps = (new_dir == current_dir) ? current_steps + 1 : 1

            # Rule: Cannot exceed max_straight steps (3) in one direction
            if new_steps > max_s
                continue
            end
            
            # Rule derived from Python's min_straight=0: Can always turn if not reversing and not exceeding max_steps.
            # This check is implicitly covered by the continue statements above.

            neighbor_state = (neighbor, new_dir, new_steps)
            new_cost = current_cost + grid.data[neighbor.y, neighbor.x]

            # Optimization: Prune path if it's already more expensive than the best known path to the goal
            if new_cost >= min_cost_to_goal
                 continue
            end

            if !haskey(cost_so_far, neighbor_state) || new_cost < cost_so_far[neighbor_state]
                 cost_so_far[neighbor_state] = new_cost
                 priority = new_cost + abs(neighbor.x - goal_coord.x) + abs(neighbor.y - goal_coord.y)
                 enqueue!(frontier, neighbor_state, priority)
            end
        end
    end

    # If min_cost_to_goal was updated, return it, otherwise return -1
    return min_cost_to_goal == typemax(Int) ? -1 : min_cost_to_goal
end

function main()
    lines = readlines("input.txt")
    result = solve(lines)
    println(result)
end

main()
