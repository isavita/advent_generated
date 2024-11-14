
function count_block_tiles(program)
    # Initialize Intcode computer state
    memory = copy(program)
    ip = 1  # instruction pointer
    relative_base = 1
    tiles = Dict{Tuple{Int,Int}, Int}()
    
    # Extend memory if needed
    function get_memory(index)
        if index > length(memory)
            append!(memory, zeros(Int, index - length(memory)))
        end
        return memory[index]
    end
    
    # Set memory value
    function set_memory!(index, value)
        if index > length(memory)
            append!(memory, zeros(Int, index - length(memory)))
        end
        memory[index] = value
    end
    
    # Parameter mode handling
    function get_param(mode, param)
        if mode == 0  # position mode
            return get_memory(param + 1)
        elseif mode == 1  # immediate mode
            return param
        elseif mode == 2  # relative mode
            return get_memory(param + relative_base)
        end
    end
    
    function set_param!(mode, param, value)
        if mode == 0  # position mode
            set_memory!(param + 1, value)
        elseif mode == 2  # relative mode
            set_memory!(param + relative_base, value)
        end
    end
    
    # Process outputs
    outputs = Int[]
    
    while true
        # Parse instruction
        instruction = get_memory(ip)
        opcode = instruction % 100
        mode1 = (instruction ÷ 100) % 10
        mode2 = (instruction ÷ 1000) % 10
        mode3 = (instruction ÷ 10000) % 10
        
        if opcode == 99  # halt
            break
        end
        
        if opcode == 1  # addition
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            set_param!(mode3, get_memory(ip + 3), param1 + param2)
            ip += 4
        elseif opcode == 2  # multiplication
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            set_param!(mode3, get_memory(ip + 3), param1 * param2)
            ip += 4
        elseif opcode == 3  # input
            # Provide default input of 0 (neutral paddle position)
            set_param!(mode1, get_memory(ip + 1), 0)
            ip += 2
        elseif opcode == 4  # output
            push!(outputs, get_param(mode1, get_memory(ip + 1)))
            ip += 2
            
            # Process tile drawing when 3 outputs are collected
            if length(outputs) == 3
                x, y, tile_id = outputs
                tiles[(x, y)] = tile_id
                empty!(outputs)
            end
        elseif opcode == 5  # jump-if-true
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            ip = param1 != 0 ? param2 + 1 : ip + 3
        elseif opcode == 6  # jump-if-false
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            ip = param1 == 0 ? param2 + 1 : ip + 3
        elseif opcode == 7  # less than
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            set_param!(mode3, get_memory(ip + 3), param1 < param2 ? 1 : 0)
            ip += 4
        elseif opcode == 8  # equals
            param1 = get_param(mode1, get_memory(ip + 1))
            param2 = get_param(mode2, get_memory(ip + 2))
            set_param!(mode3, get_memory(ip + 3), param1 == param2 ? 1 : 0)
            ip += 4
        elseif opcode == 9  # adjust relative base
            relative_base += get_param(mode1, get_memory(ip + 1))
            ip += 2
        end
    end
    
    # Count block tiles
    return count(x -> x == 2, values(tiles))
end

# Read input from file
function main()
    input = read("input.txt", String)
    program = parse.(Int, split(input, ","))
    
    block_tile_count = count_block_tiles(program)
    println("Number of block tiles: ", block_tile_count)
end

main()
