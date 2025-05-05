
using Printf

@enum EmulatorStatus Halted Output WaitingForInput
@enum Mode Explore Navigate Test

mutable struct Emulator
    memory::Dict{Int, Int}
    input::Vector{Int}
    ip::Int
    relativeBase::Int
end

function make_emulator(program::Vector{Int})
    memory = Dict{Int, Int}()
    for (i, v) in enumerate(program)
        memory[i-1] = v # 0-based indexing for memory addresses
    end
    return Emulator(memory, Int[], 0, 0)
end

function get_mem(emulator::Emulator, address::Int)
    return get(emulator.memory, address, 0)
end

function set_mem!(emulator::Emulator, address::Int, value::Int)
    emulator.memory[address] = value
end

function get_param_address(emulator::Emulator, offset::Int)
    instruction = get_mem(emulator, emulator.ip)
    mode = div(instruction, 10^(offset+1)) % 10
    param = get_mem(emulator, emulator.ip + offset)

    if mode == 0 # Position mode
        return param
    elseif mode == 2 # Relative mode
        return emulator.relativeBase + param
    else # Immediate mode (Invalid for address parameters)
        error("Invalid mode for address parameter: $mode at ip=$(emulator.ip)")
    end
end

function get_param_value(emulator::Emulator, offset::Int)
    instruction = get_mem(emulator, emulator.ip)
    mode = div(instruction, 10^(offset+1)) % 10
    param = get_mem(emulator, emulator.ip + offset)

    if mode == 0 # Position mode
        return get_mem(emulator, param)
    elseif mode == 1 # Immediate mode
        return param
    elseif mode == 2 # Relative mode
        return get_mem(emulator, emulator.relativeBase + param)
    else
        error("Invalid parameter mode: $mode at ip=$(emulator.ip)")
    end
end

function emulate!(emulator::Emulator)
    while true
        instruction = get_mem(emulator, emulator.ip)
        opcode = instruction % 100

        if opcode == 1 # ADD
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            addr = get_param_address(emulator, 3)
            set_mem!(emulator, addr, val1 + val2)
            emulator.ip += 4
        elseif opcode == 2 # MULTIPLY
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            addr = get_param_address(emulator, 3)
            set_mem!(emulator, addr, val1 * val2)
            emulator.ip += 4
        elseif opcode == 3 # INPUT
            if isempty(emulator.input)
                return (0, WaitingForInput)
            end
            addr = get_param_address(emulator, 1)
            value = popfirst!(emulator.input)
            set_mem!(emulator, addr, value)
            emulator.ip += 2
        elseif opcode == 4 # OUTPUT
            output_val = get_param_value(emulator, 1)
            emulator.ip += 2
            return (output_val, Output)
        elseif opcode == 5 # JUMP IF TRUE
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            if val1 != 0
                emulator.ip = val2
            else
                emulator.ip += 3
            end
        elseif opcode == 6 # JUMP IF FALSE
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            if val1 == 0
                emulator.ip = val2
            else
                emulator.ip += 3
            end
        elseif opcode == 7 # LESS THAN
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            addr = get_param_address(emulator, 3)
            set_mem!(emulator, addr, (val1 < val2) ? 1 : 0)
            emulator.ip += 4
        elseif opcode == 8 # EQUAL
            val1 = get_param_value(emulator, 1)
            val2 = get_param_value(emulator, 2)
            addr = get_param_address(emulator, 3)
            set_mem!(emulator, addr, (val1 == val2) ? 1 : 0)
            emulator.ip += 4
        elseif opcode == 9 # RELATIVE BASE OFFSET
            val1 = get_param_value(emulator, 1)
            emulator.relativeBase += val1
            emulator.ip += 2
        elseif opcode == 99 # HALT
            return (0, Halted)
        else
            error("Unknown opcode: $opcode at ip=$(emulator.ip)")
        end
    end
end

function write_string!(emulator::Emulator, s::String)
    for char in s
        push!(emulator.input, Int(char))
    end
end

mutable struct Room
    name::String
    connections::Dict{String, Union{Room, Nothing}} # Use Union{Room, Nothing} to represent potential null pointers
    Room(name) = new(name, Dict{String, Union{Room, Nothing}}())
end

const opposite = Dict("north" => "south", "south" => "north", "west" => "east", "east" => "west")

function find_path(start_room::Room, end_room::Room)
    queue = Tuple{Room, Vector{Room}}[(start_room, [start_room])]
    visited = Set{Room}([start_room])

    while !isempty(queue)
        current_room, path = popfirst!(queue)

        if current_room == end_room
            return path
        end

        for (_, next_room) in current_room.connections
            if next_room !== nothing && !(next_room in visited)
                push!(visited, next_room)
                new_path = vcat(path, [next_room])
                push!(queue, (next_room, new_path))
            end
        end
    end
    return nothing # Path not found
end

function main()
    program_str = read("input.txt", String)
    program = [parse(Int, s) for s in split(strip(program_str), ',')]
    emulator = make_emulator(program)

    world = Dict{String, Room}()
    inventory = Set{String}()
    mode = Explore
    path = Room[]
    checkpoint = nothing
    floor_room = nothing # Renamed from 'floor' to avoid conflict with Base.floor
    test_dir = ""

    available_items = String[]
    item_mask = UInt64(0)

    last_room = nothing
    last_items = String[]
    last_dir = ""

    output_buffer = IOBuffer()

    room_name_regex = r"^== (.+) ==$"
    list_item_regex = r"^- (.+)$"
    taken_regex = r"^You take the (.+)\.$"
    dropped_regex = r"^You drop the (.+)\.$"
    result_regex = r"(\d+) on the keypad"

    while true
        char_code, status = emulate!(emulator)

        if status == Halted
            output = String(take!(output_buffer))
            result_match = match(result_regex, output)
            if result_match !== nothing
                println(result_match.captures[1])
            else
                 println("Result not found in output.")
                 println(output) # Print full output for debugging if needed
            end
            break
        elseif status == Output
            print(output_buffer, Char(char_code))
        elseif status == WaitingForInput
            output = String(take!(output_buffer))
            # println("Output:\n", output) # Debug print

            current_room = nothing
            items_here = String[]
            local_connections = Dict{String, Nothing}()

            lines = split(output, '\n')
            i = 1
            while i <= length(lines)
                line = strip(lines[i])
                i += 1

                if isempty(line) || line == "Command?"
                    continue
                end

                m = match(room_name_regex, line)
                if m !== nothing
                    name = m.captures[1]
                    if !haskey(world, name)
                        world[name] = Room(name)
                    end
                    current_room = world[name]
                    # Skip description lines
                    while i <= length(lines) && !isempty(strip(lines[i])) && !occursin("Doors here lead:", lines[i]) && !occursin("Items here:", lines[i]) && !occursin("Command?", lines[i])
                         i += 1
                    end
                    items_here = String[] # Reset items for the new room description
                    continue
                end

                if line == "Doors here lead:"
                    fresh = isempty(current_room.connections)
                    local_connections = Dict{String, Nothing}() # Track doors seen in this output
                    while i <= length(lines) && !isempty(strip(lines[i]))
                        m_item = match(list_item_regex, strip(lines[i]))
                        if m_item !== nothing
                             dir = m_item.captures[1]
                             local_connections[dir] = nothing
                             if fresh
                                 current_room.connections[dir] = nothing
                             end
                        end
                        i += 1
                    end
                    # If not fresh, ensure all listed connections exist (they might already point to a room)
                    if !fresh
                        for dir in keys(local_connections)
                            if !haskey(current_room.connections, dir)
                                current_room.connections[dir] = nothing
                            end
                        end
                    end
                    continue
                end

                 if line == "Items here:"
                    items_here = String[] # Clear previous list for this room visit
                    while i <= length(lines) && !isempty(strip(lines[i]))
                        m_item = match(list_item_regex, strip(lines[i]))
                        if m_item !== nothing
                            push!(items_here, m_item.captures[1])
                        end
                        i += 1
                    end
                    continue
                 end

                 m_take = match(taken_regex, line)
                 if m_take !== nothing
                     taken = m_take.captures[1]
                     push!(inventory, taken)
                     # State reverts to 'last' room for item list update perspective
                     current_room = last_room
                     items_here = filter(item -> item != taken, last_items)
                     continue
                 end

                 m_drop = match(dropped_regex, line)
                 if m_drop !== nothing
                     dropped = m_drop.captures[1]
                     delete!(inventory, dropped)
                     # State reverts to 'last' room for item list update perspective
                     current_room = last_room
                     items_here = vcat(last_items, [dropped])
                     continue
                 end

                 if occursin("Alert!", line) && occursin("lighter", line) || occursin("heavier", line)
                     # This happens during the Test phase, or if we hit the checkpoint accidentally
                     if mode == Explore
                         # We found the checkpoint while exploring
                         pop!(path) # Remove the checkpoint room from the path back
                         checkpoint = last_room
                         floor_room = current_room
                         test_dir = last_dir
                         checkpoint.connections[test_dir] = floor_room # Record the connection to the floor
                         # Potentially add reverse connection if structure allows/known
                         # floor_room.connections[opposite[test_dir]] = checkpoint
                     end
                     # Reset last action, preventing incorrect room connection later
                     last_room, last_items, last_dir = nothing, String[], ""
                     continue # Don't process further standard logic for this output
                 end

                 # If a line is not recognized and not empty/command prompt
                 if !isempty(line) && !occursin("you have", lowercase(line)) && !occursin("you drop", lowercase(line)) && !occursin("you take", lowercase(line)) && !occursin("ejected", lowercase(line)) && !occursin("door", lowercase(line)) && !occursin("typing", lowercase(line))
                     println("Unrecognized line: ", line) # Keep for debugging unexpected output
                     # Consider erroring or specific handling if needed
                 end
            end # end line processing loop

            if current_room === nothing && last_room !== nothing
                # If we didn't get a room description (e.g., after drop/take), use the last known room
                current_room = last_room
                items_here = last_items # Restore items context
            elseif current_room === nothing
                 println("Error: Could not determine current room from output.")
                 println(output)
                 break # Or handle error appropriately
            end


            # Connect rooms after successful move
            if last_room !== nothing && !isempty(last_dir) && get(last_room.connections, last_dir, "missing") === nothing
                 last_room.connections[last_dir] = current_room
                 if haskey(opposite, last_dir) && !haskey(current_room.connections, opposite[last_dir]) # Avoid overwriting existing
                    current_room.connections[opposite[last_dir]] = last_room
                 end
            end

            # Update last state *before* deciding next action
            last_room, last_items, last_dir = current_room, items_here, ""

            # --- State Machine Logic ---
            if mode == Explore
                blacklist = Set([
                    "photons", "escape pod", "molten lava",
                    "infinite loop", "giant electromagnet"
                ])

                taken_something = false
                for item in items_here
                    if !(item in blacklist) && !(item in inventory)
                        command = "take $item\n"
                        # println("Cmd: ", strip(command)) # Debug
                        write_string!(emulator, command)
                        taken_something = true
                        break # Take one item per prompt cycle
                    end
                end
                if taken_something
                    continue # Loop back to process emulator output after taking
                end

                target_dir = ""
                for (dir, room) in current_room.connections
                    if room === nothing # Unexplored direction
                        push!(path, current_room)
                        target_dir = dir
                        break
                    end
                end

                if isempty(target_dir) && !isempty(path) # Need to backtrack
                    prev_room = pop!(path)
                    found_backtrack = false
                    for (dir, room) in current_room.connections
                        if room == prev_room
                            target_dir = dir
                            found_backtrack = true
                            break
                        end
                    end
                    if !found_backtrack
                        error("Cannot backtrack from $(current_room.name) to $(prev_room.name)")
                    end
                end

                if !isempty(target_dir)
                    last_dir = target_dir # Store direction *before* sending command
                    command = "$target_dir\n"
                    # println("Cmd: ", strip(command)) # Debug
                    write_string!(emulator, command)
                    continue # Loop back to process emulator output after moving
                else # Exploration complete or stuck
                    # Assumes checkpoint was found if exploration ends
                     if checkpoint === nothing
                        error("Exploration finished but checkpoint not found!")
                     end
                    path = find_path(current_room, checkpoint)
                    if path === nothing || length(path) < 2
                         error("Could not find path from $(current_room.name) to $(checkpoint.name)")
                    end
                    path = path[2:end] # Remove current room from path
                    mode = Navigate
                    # Fallthrough to Navigate logic immediately
                 end
             end # End Explore Mode

             if mode == Navigate
                if !isempty(path)
                    next_room = path[1]
                    target_dir = ""
                    for (dir, room) in current_room.connections
                        if room == next_room
                            target_dir = dir
                            break
                        end
                    end
                    if isempty(target_dir)
                        error("Cannot navigate from $(current_room.name) to $(next_room.name)")
                    end
                    popfirst!(path) # Consume step
                    command = "$target_dir\n"
                    # println("Cmd: ", strip(command)) # Debug
                    write_string!(emulator, command)
                    continue
                else # Arrived at checkpoint
                    available_items = collect(String, inventory) # Get all potentially usable items
                    sort!(available_items) # Ensure consistent order for masking
                    item_mask = UInt64(0) # Start with mask 0 (try dropping all)
                    mode = Test
                    # Fallthrough to Test logic immediately
                 end
             end # End Navigate Mode

             if mode == Test
                target_mask = item_mask
                synced = true
                for (index, item) in enumerate(available_items)
                    should_have = (target_mask & (UInt64(1) << (index - 1))) != 0
                    has_item = item in inventory
                    if should_have != has_item
                        action = should_have ? "take" : "drop"
                        command = "$action $item\n"
                        # println("Cmd: ", strip(command)) # Debug
                        write_string!(emulator, command)
                        synced = false
                        break # Sync one item per cycle
                    end
                end

                if synced
                    # Inventory matches the current mask, try moving to the floor
                    if item_mask >= (UInt64(1) << length(available_items))
                         error("Exhausted all item combinations")
                    end
                    command = "$test_dir\n"
                    # println("Cmd: ", strip(command)) # Debug
                    write_string!(emulator, command)
                    item_mask += 1 # Prepare for next combination after trying this one
                end
                continue
            end # End Test Mode
        end # End WaitingForInput handling
    end # End main while loop
end

main()
