mutable struct VM
    code::Dict{Int,Int}
    ip::Int
    input::Channel{Int}
    output::Channel{Int}
    relative_base::Int
end

function new_vm(filename::String)
    input = Channel{Int}(0)
    output = Channel{Int}(0)
    vm = VM(Dict{Int,Int}(), 0, input, output, 0)
    load_code(vm, filename)
    return vm
end

function load_code(vm::VM, filename::String)
    code = Dict{Int,Int}()
    open(filename) do file
        i = 0
        for value in split(strip(read(file, String)), ",")
            code[i] = parse(Int, value)
            i += 1
        end
    end
    vm.code = code
    vm.ip = 0
    vm.relative_base = 0
end

function run_vm(vm::VM)
    arity = 0
    while true
        cmd = vm.code[vm.ip]
        opcode = cmd % 100
        if opcode == 1
            arity = 3
            params = get_params_addresses(vm, arity)
            vm.code[params[3]] = vm.code[params[1]] + vm.code[params[2]]
        elseif opcode == 2
            arity = 3
            params = get_params_addresses(vm, arity)
            vm.code[params[3]] = vm.code[params[1]] * vm.code[params[2]]
        elseif opcode == 3
            arity = 1
            params = get_params_addresses(vm, arity)
            vm.code[params[1]] = take!(vm.input)
        elseif opcode == 4
            arity = 1
            params = get_params_addresses(vm, arity)
            put!(vm.output, vm.code[params[1]])
        elseif opcode == 5
            arity = 2
            params = get_params_addresses(vm, arity)
            if vm.code[params[1]] != 0
                vm.ip = vm.code[params[2]]
                continue
            end
        elseif opcode == 6
            arity = 2
            params = get_params_addresses(vm, arity)
            if vm.code[params[1]] == 0
                vm.ip = vm.code[params[2]]
                continue
            end
        elseif opcode == 7
            arity = 3
            params = get_params_addresses(vm, arity)
            vm.code[params[3]] = vm.code[params[1]] < vm.code[params[2]] ? 1 : 0
        elseif opcode == 8
            arity = 3
            params = get_params_addresses(vm, arity)
            vm.code[params[3]] = vm.code[params[1]] == vm.code[params[2]] ? 1 : 0
        elseif opcode == 9
            arity = 1
            params = get_params_addresses(vm, arity)
            vm.relative_base += vm.code[params[1]]
        elseif opcode == 99
            break
        else
            error("Invalid opcode: $opcode")
        end
        vm.ip += arity + 1
    end
    close(vm.input)
    close(vm.output)
end

function get_params_addresses(vm::VM, arity::Int)
    modes = digits(vm.code[vm.ip] ÷ 100, pad=arity)
    params = [get_param_address(vm, vm.ip+i, modes[i]) for i in 1:arity]
    return params
end

function get_param_address(vm::VM, pos::Int, mode::Int)
    if mode == 0
        return vm.code[pos]
    elseif mode == 1
        return pos
    elseif mode == 2
        return vm.relative_base + vm.code[pos]
    else
        error("Invalid mode: $mode")
    end
end

function send_string(ch::Channel{Int}, s::String)
    for c in s
        put!(ch, Int(c))
    end
    put!(ch, Int('\n'))
end

function reader(input::Channel{Int}, done::Channel{Nothing})
    for c in input
        if c > 127
            println(c)
            break
        end
    end
    put!(done, nothing)
end

function main()
    done = Channel{Nothing}(1)
    vm = new_vm("input.txt")
    @async run_vm(vm)
    @async reader(vm.output, done)
    instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN"
    ]
    for i in instructions
        send_string(vm.input, i)
    end
    take!(done)
end

main()