
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, {})
    for word in line:gmatch("%S+") do
        table.insert(instructions[#instructions], word)
    end
end
file:close()

local registers0 = {p = 0}
local registers1 = {p = 1}
local queue0 = {}
local queue1 = {}
local sendCount1 = 0
local i0, i1 = 1, 1
local deadlock0, deadlock1 = false, false

while not (deadlock0 and deadlock1) do
    deadlock0, deadlock1 = true, true

    -- Program 0
    while i0 <= #instructions do
        local instruction = instructions[i0]
        local cmd = instruction[1]
        local arg1 = instruction[2]
        if cmd == "snd" then
            table.insert(queue1, tonumber(arg1) or registers0[arg1])
        elseif cmd == "set" then
            registers0[arg1] = tonumber(instruction[3]) or registers0[instruction[3]]
        elseif cmd == "add" then
            registers0[arg1] = (registers0[arg1] or 0) + (tonumber(instruction[3]) or registers0[instruction[3]])
        elseif cmd == "mul" then
            registers0[arg1] = (registers0[arg1] or 0) * (tonumber(instruction[3]) or registers0[instruction[3]])
        elseif cmd == "mod" then
            registers0[arg1] = (registers0[arg1] or 0) % (tonumber(instruction[3]) or registers0[instruction[3]])
        elseif cmd == "rcv" then
            if #queue0 == 0 then
                break
            end
            registers0[arg1] = table.remove(queue0, 1)
        elseif cmd == "jgz" then
            if (tonumber(arg1) or registers0[arg1]) > 0 then
                i0 = i0 + (tonumber(instruction[3]) or registers0[instruction[3]])
                deadlock0 = false
                break
            end
        end
        i0 = i0 + 1
        deadlock0 = false
    end

    -- Program 1
    while i1 <= #instructions do
        local instruction = instructions[i1]
        local cmd = instruction[1]
        local arg1 = instruction[2]
        if cmd == "snd" then
            table.insert(queue0, tonumber(arg1) or registers1[arg1])
            sendCount1 = sendCount1 + 1
        elseif cmd == "set" then
            registers1[arg1] = tonumber(instruction[3]) or registers1[instruction[3]]
        elseif cmd == "add" then
            registers1[arg1] = (registers1[arg1] or 0) + (tonumber(instruction[3]) or registers1[instruction[3]])
        elseif cmd == "mul" then
            registers1[arg1] = (registers1[arg1] or 0) * (tonumber(instruction[3]) or registers1[instruction[3]])
        elseif cmd == "mod" then
            registers1[arg1] = (registers1[arg1] or 0) % (tonumber(instruction[3]) or registers1[instruction[3]])
        elseif cmd == "rcv" then
            if #queue1 == 0 then
                break
            end
            registers1[arg1] = table.remove(queue1, 1)
        elseif cmd == "jgz" then
            if (tonumber(arg1) or registers1[arg1]) > 0 then
                i1 = i1 + (tonumber(instruction[3]) or registers1[instruction[3]])
                deadlock1 = false
                break
            end
        end
        i1 = i1 + 1
        deadlock1 = false
    end
end

print(sendCount1)
