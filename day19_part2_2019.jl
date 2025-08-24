mutable struct VM
    Code::Dict{Int,Int}
    Ip::Int
    Input::Channel{Int}
    Output::Channel{Int}
    RelativeBase::Int
end

function NewVM(filename::String)
    input = Channel{Int}(Inf)
    output = Channel{Int}(Inf)
    vm = VM(Dict{Int,Int}(), 0, input, output, 0)
    Load(vm, filename)
    return vm
end

function Load(vm::VM, filename::String)
    code = Dict{Int,Int}()
    open(filename) do file
        i = 0
        for value in split(strip(read(file, String)), ",")
            code[i] = parse(Int, value)
            i += 1
        end
    end
    vm.Code = code
    vm.Ip = 0
    vm.RelativeBase = 0
end

function Run(vm::VM)
    while true
        cmd = get(vm.Code, vm.Ip, 0)
        opcode = cmd % 100
        if opcode == 1
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = get(vm.Code, params[1], 0) + get(vm.Code, params[2], 0)
        elseif opcode == 2
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = get(vm.Code, params[1], 0) * get(vm.Code, params[2], 0)
        elseif opcode == 3
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[1]] = take!(vm.Input)
        elseif opcode == 4
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            put!(vm.Output, get(vm.Code, params[1], 0))
        elseif opcode == 5
            arity = 2
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            if get(vm.Code, params[1], 0) != 0
                vm.Ip = get(vm.Code, params[2], 0)
                continue
            end
        elseif opcode == 6
            arity = 2
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            if get(vm.Code, params[1], 0) == 0
                vm.Ip = get(vm.Code, params[2], 0)
                continue
            end
        elseif opcode == 7
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = get(vm.Code, params[1], 0) < get(vm.Code, params[2], 0) ? 1 : 0
        elseif opcode == 8
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = get(vm.Code, params[1], 0) == get(vm.Code, params[2], 0) ? 1 : 0
        elseif opcode == 9
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.RelativeBase += get(vm.Code, params[1], 0)
        elseif opcode == 99
            break
        else
            error("Invalid opcode: $opcode")
        end
        vm.Ip += arity + 1
    end
end

function getParamsAddresses(vm::VM, pos::Int, cmd::Int, arity::Int)
    modes = digits(cmd ÷ 100, pad=arity)
    results = Vector{Int}(undef, arity)
    for i in 1:arity
        results[i] = getParamAddress(vm, pos + i, modes[i])
    end
    return results
end

function getParamAddress(vm::VM, pos::Int, mode::Int)
    if mode == 0
        return get(vm.Code, pos, 0)
    elseif mode == 1
        return pos
    elseif mode == 2
        return vm.RelativeBase + get(vm.Code, pos, 0)
    else
        error("Invalid mode: $mode")
    end
end

function Beam(x::Int, y::Int)
    vm = NewVM("input.txt")
    @async Run(vm)
    put!(vm.Input, x)
    put!(vm.Input, y)
    beam = take!(vm.Output)
    return beam == 1
end

function main()
    y = 20
    x = 0
    while true
        if !Beam(x, y)
            x += 1
            continue
        end
        if !Beam(x + 99, y)
            y += 1
            continue
        end
        if !Beam(x, y + 99)
            x += 1
            continue
        end
        println(x * 10000 + y)
        break
    end
end

main()