function hexToBin(hex)
    local bin = ""
    local hexToBinMap = {
        ["0"] = "0000", ["1"] = "0001", ["2"] = "0010", ["3"] = "0011",
        ["4"] = "0100", ["5"] = "0101", ["6"] = "0110", ["7"] = "0111",
        ["8"] = "1000", ["9"] = "1001", ["A"] = "1010", ["B"] = "1011",
        ["C"] = "1100", ["D"] = "1101", ["E"] = "1110", ["F"] = "1111"
    }
    for i = 1, #hex do
        local h = hex:sub(i, i)
        bin = bin .. hexToBinMap[h:upper()]
    end
    return bin
end

function parsePacket(binStr, idx)
    local version = tonumber(binStr:sub(idx+1, idx+3), 2)
    local typeID = tonumber(binStr:sub(idx+4, idx+6), 2)
    idx = idx + 6

    if typeID == 4 then
        local done = false
        repeat
            local flag = binStr:sub(idx+1, idx+1)
            idx = idx + 5
            done = flag == '0'
        until done
        return version, idx
    end

    local lengthTypeID = tonumber(binStr:sub(idx+1, idx+1), 2)
    idx = idx + 1
    local numSubPackets, subPacketLength = 0, 0

    if lengthTypeID == 0 then
        subPacketLength = tonumber(binStr:sub(idx+1, idx+15), 2)
        idx = idx + 15
    else
        numSubPackets = tonumber(binStr:sub(idx+1, idx+11), 2)
        idx = idx + 11
    end

    local versionSum = version
    if lengthTypeID == 0 then
        local targetIdx = idx + subPacketLength
        while idx < targetIdx do
            local subVersion, newIndex = parsePacket(binStr, idx)
            versionSum = versionSum + subVersion
            idx = newIndex
        end
    else
        for i = 1, numSubPackets do
            local subVersion, newIndex = parsePacket(binStr, idx)
            versionSum = versionSum + subVersion
            idx = newIndex
        end
    end
    return versionSum, idx
end

local file = io.open("input.txt", "r")
if not file then
    error("Error reading file")
end
local data = file:read("*all")
file:close()

local hexStr = data:gsub("%s+", "")
local binStr = hexToBin(hexStr)
local versionSum, _ = parsePacket(binStr, 0)
print(versionSum)