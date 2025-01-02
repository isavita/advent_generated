
function load_program(filename)
    program = map(x -> parse(Int, x), split(strip(read(filename, String)), ","))
    return program
end

function run_vm(program, input_channel, output_channel)
    code = copy(program)
    ip = 1
    
    function get_param(address, immediate)
        param = code[address]
        return immediate ? param : code[param + 1]
    end

    while true
        cmd = code[ip]
        opcode = cmd % 100

        if opcode == 1
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            address = get_param(ip + 3, true) + 1
            code[address] = param1 + param2
            ip += 4
        elseif opcode == 2
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            address = get_param(ip + 3, true) + 1
            code[address] = param1 * param2
            ip += 4
        elseif opcode == 3
            address = get_param(ip + 1, true) + 1
            code[address] = take!(input_channel)
            ip += 2
        elseif opcode == 4
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            put!(output_channel, param1)
            ip += 2
        elseif opcode == 5
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            if param1 != 0
                ip = param2 + 1
            else
                ip += 3
            end
        elseif opcode == 6
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            if param1 == 0
                ip = param2 + 1
            else
                ip += 3
            end
        elseif opcode == 7
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            address = get_param(ip + 3, true) + 1
            code[address] = param1 < param2 ? 1 : 0
            ip += 4
        elseif opcode == 8
            param1 = get_param(ip + 1, cmd ÷ 100 % 10 == 1)
            param2 = get_param(ip + 2, cmd ÷ 1000 % 10 == 1)
            address = get_param(ip + 3, true) + 1
            code[address] = param1 == param2 ? 1 : 0
            ip += 4
        elseif opcode == 99
            return
        else
            error("Invalid opcode: $opcode")
        end
    end
end

function permutations(arr)
    n = length(arr)
    result = Vector{typeof(arr)}()
    
    function heap_permute(a, size)
        if size == 1
            push!(result, copy(a))
            return
        end
        
        for i in 1:size
            heap_permute(a, size - 1)
            if size % 2 == 1
                a[i], a[size] = a[size], a[i]
            else
                a[1], a[size] = a[size], a[1]
            end
        end
    end
    
    heap_permute(arr, n)
    return result
end

function run_loop(phase)
    program = load_program("input.txt")
    channels = [Channel{Int}(1) for _ in 1:6]
    
    for i in 1:5
        put!(channels[i], phase[i])
    end
    
    tasks = [@task run_vm(program, channels[i], channels[i % 5 + 1]) for i in 1:5]
    foreach(schedule, tasks)
    
    put!(channels[1], 0)
    
    for task in tasks
        wait(task)
    end
    
    return take!(channels[1])
end

function main()
    max_output = 0
    for phase in permutations([5, 6, 7, 8, 9])
        output = run_loop(phase)
        max_output = max(max_output, output)
    end
    println(max_output)
end

main()
