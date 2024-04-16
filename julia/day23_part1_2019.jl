mutable struct VM
    Code::Dict{Int,Int}
    Ip::Int
    Input::Channel{Int}
    Output::Channel{Int}
    RelativeBase::Int
end

function NewVM(filename::String, chanSize::Int)
    input = Channel{Int}(chanSize)
    output = Channel{Int}(chanSize)
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
    arity = 0
    while true
        cmd = vm.Code[vm.Ip]
        opcode = cmd % 100
        if opcode == 1
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = vm.Code[params[1]] + vm.Code[params[2]]
        elseif opcode == 2
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = vm.Code[params[1]] * vm.Code[params[2]]
        elseif opcode == 3
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[1]] = take!(vm.Input)
        elseif opcode == 4
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            put!(vm.Output, vm.Code[params[1]])
        elseif opcode == 5
            arity = 2
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            if vm.Code[params[1]] != 0
                vm.Ip = vm.Code[params[2]]
                continue
            end
        elseif opcode == 6
            arity = 2
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            if vm.Code[params[1]] == 0
                vm.Ip = vm.Code[params[2]]
                continue
            end
        elseif opcode == 7
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = vm.Code[params[1]] < vm.Code[params[2]] ? 1 : 0
        elseif opcode == 8
            arity = 3
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.Code[params[3]] = vm.Code[params[1]] == vm.Code[params[2]] ? 1 : 0
        elseif opcode == 9
            arity = 1
            params = getParamsAddresses(vm, vm.Ip, cmd, arity)
            vm.RelativeBase += vm.Code[params[1]]
        elseif opcode == 99
            close(vm.Input)
            close(vm.Output)
            break
        else
            error("Invalid opcode $opcode")
        end
        vm.Ip += arity + 1
    end
end

function getParamsAddresses(vm::VM, pos::Int, cmd::Int, arity::Int)
    modes = digits(cmd ÷ 100, pad=arity)
    results = Vector{Int}(undef, arity)
    for i in 1:arity
        results[i] = getParamAddress(vm, pos+i, modes[i])
    end
    return results
end

function getParamAddress(vm::VM, pos::Int, mode::Int)
    if mode == 0
        return vm.Code[pos]
    elseif mode == 1
        return pos
    elseif mode == 2
        return vm.RelativeBase + vm.Code[pos]
    else
        error("Invalid mode $mode")
    end
end

const Size = 50

function main()
    done = Channel{Nothing}(1)
    vms = Vector{VM}(undef, Size)
    for i in 1:Size
        vms[i] = NewVM("input.txt", 10000)
        @async Run(vms[i])
        put!(vms[i].Input, i-1)
        put!(vms[i].Input, -1)
    end
    for i in 1:Size
        @async router(vms, i, done)
    end
    take!(done)
end

function router(vms::Vector{VM}, id::Int, done::Channel{Nothing})
    vm = vms[id]
    while true
        address = take!(vm.Output)
        x = take!(vm.Output)
        y = take!(vm.Output)
        if address == 255
            println(y)
            put!(done, nothing)
            return
        end
        put!(vms[address+1].Input, x)
        put!(vms[address+1].Input, y)
    end
end

main()