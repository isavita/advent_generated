
function load(filename)
    s = read(filename, String)
    parse.(Int, split(strip(s), ","))
end

function run(code, input_channel, output_channel)
    ip = 1
    while true
        cmd = code[ip]
        opcode = cmd % 100
        immediate = (paramNum) -> (cmd ÷ (10^(paramNum + 1))) % 10 == 1
        get_param = (address, imm) -> imm ? code[address] : code[code[address]+1]
        if opcode == 1
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            address = code[ip+3] + 1
            code[address] = param1 + param2
            ip += 4
        elseif opcode == 2
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            address = code[ip+3] + 1
            code[address] = param1 * param2
            ip += 4
        elseif opcode == 3
            address = code[ip+1] + 1
            code[address] = take!(input_channel)
            ip += 2
        elseif opcode == 4
            param1 = get_param(ip + 1, immediate(1))
            put!(output_channel, param1)
            ip += 2
        elseif opcode == 5
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            if param1 != 0
                ip = param2 + 1
            else
                ip += 3
            end
        elseif opcode == 6
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            if param1 == 0
                ip = param2 + 1
            else
                ip += 3
            end
        elseif opcode == 7
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            address = code[ip+3] + 1
            code[address] = param1 < param2 ? 1 : 0
            ip += 4
        elseif opcode == 8
            param1 = get_param(ip + 1, immediate(1))
            param2 = get_param(ip + 2, immediate(2))
            address = code[ip+3] + 1
            code[address] = param1 == param2 ? 1 : 0
            ip += 4
        elseif opcode == 99
            return
        else
            error("not an opcode $cmd")
        end
    end
end

function permutations(arr)
    res = Vector{Int}[]
    function helper(arr, n)
        if n == 1
            push!(res, copy(arr))
        else
            for i in 1:n
                helper(arr, n - 1)
                if n % 2 == 1
                    arr[i], arr[n] = arr[n], arr[i]
                else
                    arr[1], arr[n] = arr[n], arr[1]
                end
            end
        end
    end
    helper(arr, length(arr))
    res
end

function run5(phase)
    chs = [Channel{Int}(1) for _ in 1:6]
    for i in 1:5
        put!(chs[i], phase[i])
        code = load("input.txt")
        @async run(code, chs[i], chs[i+1])
    end
    put!(chs[1], 0)
    take!(chs[6])
end

function main()
    max_val = 0
    for phase in permutations([0, 1, 2, 3, 4])
        res = run5(phase)
        max_val = max(max_val, res)
    end
    println(max_val)
end

main()
