
function load_program(filename)
    code = Dict{Int, Int}()
    for (i, v) in enumerate(parse.(Int, split(strip(read(filename, String)), ",")))
        code[i-1] = v
    end
    code
end

function run_vm(code, input_channel, output_channel)
    ip = 0
    relative_base = 0
    
    get_param_address = function(pos, mode)
        if mode == 0
            return get(code, pos, 0)
        elseif mode == 1
            return pos
        elseif mode == 2
            return relative_base + get(code, pos, 0)
        end
    end

    while true
        cmd = code[ip]
        opcode = cmd % 100
        modes = [((cmd ÷ 100) ÷ (10^i)) % 10 for i in 0:2]

        if opcode == 1
            params = [get_param_address(ip+i, modes[i]) for i in 1:3]
            code[params[3]] = get(code, params[1], 0) + get(code, params[2], 0)
            ip += 4
        elseif opcode == 2
            params = [get_param_address(ip+i, modes[i]) for i in 1:3]
            code[params[3]] = get(code, params[1], 0) * get(code, params[2], 0)
            ip += 4
        elseif opcode == 3
            params = [get_param_address(ip+i, modes[i]) for i in 1:1]
            code[params[1]] = take!(input_channel)
            ip += 2
        elseif opcode == 4
            params = [get_param_address(ip+i, modes[i]) for i in 1:1]
            put!(output_channel, get(code, params[1], 0))
            ip += 2
        elseif opcode == 5
            params = [get_param_address(ip+i, modes[i]) for i in 1:2]
            if get(code, params[1], 0) != 0
                ip = get(code, params[2], 0)
            else
                ip += 3
            end
        elseif opcode == 6
            params = [get_param_address(ip+i, modes[i]) for i in 1:2]
            if get(code, params[1], 0) == 0
                ip = get(code, params[2], 0)
            else
                ip += 3
            end
        elseif opcode == 7
            params = [get_param_address(ip+i, modes[i]) for i in 1:3]
            code[params[3]] = get(code, params[1], 0) < get(code, params[2], 0) ? 1 : 0
            ip += 4
        elseif opcode == 8
            params = [get_param_address(ip+i, modes[i]) for i in 1:3]
            code[params[3]] = get(code, params[1], 0) == get(code, params[2], 0) ? 1 : 0
            ip += 4
        elseif opcode == 9
            params = [get_param_address(ip+i, modes[i]) for i in 1:1]
            relative_base += get(code, params[1], 0)
            ip += 2
        elseif opcode == 99
            return
        end
    end
end

function router(vms, id, nat_channel)
    vm = vms[id+1]
    while true
        address = take!(vm[2])
        x = take!(vm[2])
        y = take!(vm[2])
        if address == 255
            put!(nat_channel, x)
            put!(nat_channel, y)
        else
            put!(vms[address+1][1], x)
            put!(vms[address+1][1], y)
        end
    end
end

function nat(vms, nat_channel)
    x = -1
    y = -1

    @async while true
        x = take!(nat_channel)
        y = take!(nat_channel)
    end

    last_y = -1
    while true
        sleep(0.01)
        if x != -1 && all(isempty.(getindex.(vms, 1)))
            put!(vms[1][1], x)
            put!(vms[1][1], y)
            if y == last_y
                return y
            end
            last_y = y
        end
    end
end

const SIZE = 50
vms = [(Channel{Int}(10000), Channel{Int}(10000)) for _ in 1:SIZE]
nat_channel = Channel{Int}(10000)

for i in 0:SIZE-1
    code = load_program("input.txt")
    @async run_vm(code, vms[i+1][1], vms[i+1][2])
    put!(vms[i+1][1], i)
    put!(vms[i+1][1], -1)
end

for i in 0:SIZE-1
    @async router(vms, i, nat_channel)
end

println(nat(vms, nat_channel))
