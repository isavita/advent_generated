
using Printf

mutable struct VM
    code::Dict{Int, Int}
    ip::Int
    input::Vector{Int}
    output::Vector{Int}
    relative_base::Int
end

function VM(filename::String)
    code = Dict{Int, Int}()
    open(filename, "r") do file
        line = strip(read(file, String))
        list_str = split(line, ',')
        for (i, val_str) in enumerate(list_str)
            code[i-1] = parse(Int, val_str) # 0-based indexing for code addresses
        end
    end
    return VM(code, 0, Int[], Int[], 0)
end

function get_param(vm::VM, modes::Int, index::Int)
    mode = (modes ÷ (10^(index-1))) % 10
    param_val = get(vm.code, vm.ip + index, 0)

    if mode == 0 # Position mode
        return get(vm.code, param_val, 0)
    elseif mode == 1 # Immediate mode
        return param_val
    elseif mode == 2 # Relative mode
        return get(vm.code, vm.relative_base + param_val, 0)
    else
        error("Unknown parameter mode: $mode")
    end
end

function get_address(vm::VM, modes::Int, index::Int)
    mode = (modes ÷ (10^(index-1))) % 10
    param_val = get(vm.code, vm.ip + index, 0)

    if mode == 0 # Position mode
        return param_val
    elseif mode == 2 # Relative mode
        return vm.relative_base + param_val
    else
        error("Invalid address mode for write: $mode")
    end
end

function run!(vm::VM)
    while true
        cmd = get(vm.code, vm.ip, 0)
        opcode = cmd % 100
        modes = cmd ÷ 100

        if opcode == 1 # add
            addr = get_address(vm, modes, 3)
            vm.code[addr] = get_param(vm, modes, 1) + get_param(vm, modes, 2)
            vm.ip += 4
        elseif opcode == 2 # multiply
            addr = get_address(vm, modes, 3)
            vm.code[addr] = get_param(vm, modes, 1) * get_param(vm, modes, 2)
            vm.ip += 4
        elseif opcode == 3 # read
             if isempty(vm.input)
                 error("Input queue is empty")
             end
             addr = get_address(vm, modes, 1)
             vm.code[addr] = popfirst!(vm.input)
             vm.ip += 2
        elseif opcode == 4 # write
            val = get_param(vm, modes, 1)
            push!(vm.output, val)
            vm.ip += 2
        elseif opcode == 5 # jump-if-true
            if get_param(vm, modes, 1) != 0
                vm.ip = get_param(vm, modes, 2)
            else
                vm.ip += 3
            end
        elseif opcode == 6 # jump-if-false
            if get_param(vm, modes, 1) == 0
                vm.ip = get_param(vm, modes, 2)
            else
                vm.ip += 3
            end
        elseif opcode == 7 # less than
            addr = get_address(vm, modes, 3)
            vm.code[addr] = (get_param(vm, modes, 1) < get_param(vm, modes, 2)) ? 1 : 0
            vm.ip += 4
        elseif opcode == 8 # equals
            addr = get_address(vm, modes, 3)
            vm.code[addr] = (get_param(vm, modes, 1) == get_param(vm, modes, 2)) ? 1 : 0
            vm.ip += 4
        elseif opcode == 9 # adjust relative base
            vm.relative_base += get_param(vm, modes, 1)
            vm.ip += 2
        elseif opcode == 99 # halt
            break
        else
            error("Unknown opcode $opcode at ip $(vm.ip)")
        end
    end
end

function send_string!(vm::VM, s::String)
    for char in s
        push!(vm.input, Int(char))
    end
    push!(vm.input, Int('\n'))
end

function main()
    vm = VM("input.txt")
    instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK",
    ]
    for instruction in instructions
        send_string!(vm, instruction)
    end
    run!(vm)

    for output_val in vm.output
        if output_val > 127
            println(output_val)
            break
        # else # Optional: print ASCII output for debugging
        #    print(Char(output_val))
        end
    end
    #println() # Add newline if printing ASCII
end

main()
