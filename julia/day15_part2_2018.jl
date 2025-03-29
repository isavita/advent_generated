
using Printf
using DataStructures # For Queue used in BFS

@enum Kind KIND_SPACE KIND_ELF KIND_GOBLIN KIND_WALL

const RUNE_KINDS = Dict{Char,Kind}(
    '.' => KIND_SPACE,
    'E' => KIND_ELF,
    'G' => KIND_GOBLIN,
    '#' => KIND_WALL,
)

const OFFSETS = [(0, -1), (-1, 0), (1, 0), (0, 1)] # Reading order: Up, Left, Right, Down

const DEFAULT_HITPOINTS = 200
const DEFAULT_POWER = 3

mutable struct Tile
    kind::Kind
    x::Int
    y::Int
    unit::Union{Nothing,Any} # Using Any to avoid circular type definition with Unit, will hold Unit
end

mutable struct Unit
    kind::Kind
    hitpoints::Int
    power::Int
    tile::Tile
end

mutable struct Cave
    map::Matrix{Tile}
    units::Vector{Unit}
    height::Int
    width::Int
end

function is_unit(kind::Kind)
    return kind == KIND_ELF || kind == KIND_GOBLIN
end

function Base.show(io::IO, t::Tile)
    if t.kind == KIND_WALL
        print(io, "#")
    elseif t.kind == KIND_SPACE
        print(io, ".")
    elseif t.kind == KIND_ELF
        print(io, "E")
    elseif t.kind == KIND_GOBLIN
        print(io, "G")
    end
end

function Base.show(io::IO, u::Unit)
    kind_char = u.kind == KIND_ELF ? 'E' : 'G'
    print(io, "$kind_char($(u.tile.x),$(u.tile.y)) HP:$(u.hitpoints) P:$(u.power)")
end

# --- Parsing ---

function parse_map(input::Vector{String}, elf_power::Int)::Cave
    height = length(input)
    width = length(input[1])
    map_matrix = Matrix{Tile}(undef, height, width)
    units_list = Vector{Unit}()
    
    for y in 1:height
        for x in 1:width
            char = input[y][x]
            kind = RUNE_KINDS[char]
            tile = Tile(kind, x, y, nothing)
            map_matrix[y, x] = tile
            if is_unit(kind)
                power = (kind == KIND_ELF) ? elf_power : DEFAULT_POWER
                unit = Unit(kind, DEFAULT_HITPOINTS, power, tile)
                tile.unit = unit
                push!(units_list, unit)
            end
        end
    end
    return Cave(map_matrix, units_list, height, width)
end

# --- Tile Helpers ---

function get_neighbors(tile::Tile, cave::Cave)::Vector{Tile}
    neighbors = Vector{Tile}()
    for (dx, dy) in OFFSETS
        nx, ny = tile.x + dx, tile.y + dy
        if 1 <= ny <= cave.height && 1 <= nx <= cave.width
             push!(neighbors, cave.map[ny, nx])
        end
    end
    return neighbors
end


function get_walkable_neighbors(tile::Tile, cave::Cave)::Vector{Tile}
    neighbors = Vector{Tile}()
    for (dx, dy) in OFFSETS
        nx, ny = tile.x + dx, tile.y + dy
        if 1 <= ny <= cave.height && 1 <= nx <= cave.width
            neighbor_tile = cave.map[ny, nx]
            if neighbor_tile.kind == KIND_SPACE
                push!(neighbors, neighbor_tile)
            end
        end
    end
    return neighbors
end

# --- Unit Helpers ---

position(u::Unit) = (u.tile.y, u.tile.x)

function has_targets(unit::Unit, cave::Cave)::Bool
    for other_unit in cave.units
        if other_unit.hitpoints > 0 && other_unit.kind != unit.kind
            return true
        end
    end
    return false
end

function get_enemies(unit::Unit, cave::Cave)::Vector{Unit}
    enemies = Vector{Unit}()
    for other_unit in cave.units
        if other_unit.hitpoints > 0 && other_unit.kind != unit.kind
            push!(enemies, other_unit)
        end
    end
    sort!(enemies, by=position)
    return enemies
end

function find_walkable_tiles(cave::Cave, start_tile::Tile)::Tuple{Dict{Tile,Int},Dict{Tile,Union{Tile,Nothing}}}
    frontier = Queue{Tile}()
    enqueue!(frontier, start_tile)
    distance = Dict{Tile,Int}(start_tile => 0)
    came_from = Dict{Tile,Union{Tile,Nothing}}(start_tile => nothing)

    while !isempty(frontier)
        current = dequeue!(frontier)
        for next_tile in get_walkable_neighbors(current, cave)
            if !haskey(distance, next_tile)
                distance[next_tile] = distance[current] + 1
                came_from[next_tile] = current
                enqueue!(frontier, next_tile)
            end
        end
    end
    return distance, came_from
end

function find_next_tile(unit::Unit, cave::Cave)::Union{Tile, Nothing}
    enemies = get_enemies(unit, cave)
    if isempty(enemies)
        return nothing # Should not happen if has_targets is checked first
    end

    target_squares = Set{Tile}()
    for enemy in enemies
        union!(target_squares, get_walkable_neighbors(enemy.tile, cave))
    end

    if isempty(target_squares)
        return nothing # No reachable squares next to enemies
    end
    
    distances, came_from = find_walkable_tiles(cave, unit.tile)

    reachable_targets = Vector{Tuple{Int, Int, Int, Tile}}() # (dist, y, x, tile)
    min_dist = typemax(Int)

    for target_tile in target_squares
        if haskey(distances, target_tile)
            dist = distances[target_tile]
            if dist < min_dist
                 min_dist = dist
                 empty!(reachable_targets) # Clear previous targets with larger distance
            end
            if dist == min_dist
                 push!(reachable_targets, (dist, target_tile.y, target_tile.x, target_tile))
            end
        end
    end


    if isempty(reachable_targets)
        return nothing # Cannot reach any target square
    end

    sort!(reachable_targets) # Sort by dist, then y, then x
    chosen_target_tile = reachable_targets[1][4]

    # Backtrack to find the first step towards the chosen target
    current = chosen_target_tile
    while came_from[current] != unit.tile
         prev = came_from[current]
         if prev === nothing # Should not happen if target is reachable
              # This case might happen if the unit is already adjacent
              # to the chosen_target_tile, but let's double check logic.
              # If chosen_target_tile is a neighbor, came_from[chosen_target_tile] should be unit.tile
              @warn "Path reconstruction error for unit $unit to target $chosen_target_tile"
              return nothing # Or maybe return chosen_target_tile if it's adjacent? Let's assume error.
         end
         current = prev
    end
    
    return current # This is the first step tile
end


function find_enemy_neighbor(unit::Unit, cave::Cave)::Union{Unit, Nothing}
    target = nothing
    min_hp = typemax(Int)

    for neighbor_tile in get_neighbors(unit.tile, cave)
         if neighbor_tile.unit !== nothing && neighbor_tile.unit.kind != unit.kind && neighbor_tile.unit.hitpoints > 0
             enemy = neighbor_tile.unit
             if enemy.hitpoints < min_hp
                 min_hp = enemy.hitpoints
                 target = enemy
             # Tie-breaking by reading order (y, then x) is implicitly handled by OFFSETS order
             elseif enemy.hitpoints == min_hp && target !== nothing
                 if position(enemy) < position(target)
                    target = enemy
                 end
             end
        end
    end
    return target
end


function move!(unit::Unit, cave::Cave)
    # Check if already adjacent to an enemy
    if find_enemy_neighbor(unit, cave) !== nothing
        return
    end

    next_move_tile = find_next_tile(unit, cave)

    if next_move_tile !== nothing
        # Update old tile
        unit.tile.kind = KIND_SPACE
        unit.tile.unit = nothing

        # Update unit's tile reference
        unit.tile = next_move_tile

        # Update new tile
        unit.tile.kind = unit.kind
        unit.tile.unit = unit
    end
end

function attack!(unit::Unit, cave::Cave)::Bool # Returns true if an elf died
    enemy = find_enemy_neighbor(unit, cave)
    if enemy !== nothing
        enemy_died = damage!(enemy, cave, unit.power)
        return enemy_died && enemy.kind == KIND_ELF
    end
    return false # No attack occurred or no elf died
end

function damage!(unit::Unit, cave::Cave, damage_amount::Int)::Bool # Returns true if unit died
    unit.hitpoints -= damage_amount
    if unit.hitpoints <= 0
        remove_unit!(cave, unit)
        return true
    end
    return false
end

# --- Cave Helpers ---

function get_status(cave::Cave)::Tuple{Int, Bool}
    elves = false
    goblins = false
    total_hp = 0
    for u in cave.units
        if u.hitpoints <= 0
            continue
        end
        if u.kind == KIND_ELF
            elves = true
        else # KIND_GOBLIN
            goblins = true
        end
        total_hp += u.hitpoints
    end
    return total_hp, elves && goblins # Combat continues if both sides have units
end

function remove_unit!(cave::Cave, unit::Unit)
    # Clear the tile the unit occupied
    if unit.tile !== nothing # Should always be true unless already removed?
         unit.tile.kind = KIND_SPACE
         unit.tile.unit = nothing
         # unit.tile = nothing # Don't nullify unit's tile ref, just mark tile as empty
    end
    # The unit will be fully removed from cave.units in remove_the_dead!
end


function remove_the_dead!(cave::Cave)
     filter!(u -> u.hitpoints > 0, cave.units)
end


function tick!(cave::Cave, stop_on_elf_death::Bool)::Tuple{Bool, Bool} # Returns (combat_continues, elf_died_this_tick)
    remove_the_dead!(cave) # Clean up from previous round if needed
    sort!(cave.units, by=position) # Sort by reading order (y, then x)

    any_elf_died = false

    # Need to iterate carefully as units list might change if one dies mid-tick
    unit_indices = 1:length(cave.units)
    for i in unit_indices
        # Check if unit still exists and is alive (could have died earlier in this tick)
        if i > length(cave.units) || cave.units[i].hitpoints <= 0
            continue
        end
        
        unit = cave.units[i]

        # Check if combat should end immediately (no targets left for this unit)
        if !has_targets(unit, cave)
            return false, any_elf_died # Combat ends
        end

        move!(unit, cave)
        elf_died_in_attack = attack!(unit, cave)

        if elf_died_in_attack
            any_elf_died = true
            if stop_on_elf_death
                 return false, true # Combat ends immediately because an elf died
            end
        end
    end

    # Check overall combat status after the full round
    _, combat_ongoing = get_status(cave)
    return combat_ongoing, any_elf_died
end


# --- Main Simulation Logic ---

function run_simulation(input::Vector{String}, elf_power::Int, stop_on_elf_death::Bool)::Tuple{Int, Bool} # Returns (outcome, elf_died)
    cave = parse_map(input, elf_power)
    rounds = 0
    elf_died_during_sim = false

    while true
        combat_ongoing, elf_died_this_tick = tick!(cave, stop_on_elf_death)
        
        if elf_died_this_tick
            elf_died_during_sim = true
            if stop_on_elf_death
                 # Simulation stops early, outcome doesn't matter for part 2 in this case
                 return -1, true 
            end
        end

        if !combat_ongoing
            # Combat finished. Calculate outcome.
            final_hp, _ = get_status(cave)
            return rounds * final_hp, elf_died_during_sim
        end
        
        rounds += 1
    end
end

function cheating_elves(input::Vector{String})::Int
    elf_power = DEFAULT_POWER + 1 # Start checking from power 4
    while true
        # Need to create a fresh cave for each power level attempt
        outcome, elf_died = run_simulation(input, elf_power, true) # Pass stop_on_elf_death = true
        
        if !elf_died
            return outcome # Found the lowest power where no elves die
        end

        elf_power += 1
    end
end


# --- Entry Point ---
function main()
    lines = readlines("input.txt")
    result = cheating_elves(lines)
    println(result)
end

main()

