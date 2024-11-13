
local function hexToBin(hex)
    local bin = {}
    for i = 1, #hex do
        local b = tonumber(hex:sub(i,i), 16)
        local bits = {}
        for j = 3, 0, -1 do
            bits[#bits+1] = math.floor(b / (2^j)) % 2
        end
        for _, bit in ipairs(bits) do
            bin[#bin+1] = tostring(bit)
        end
    end
    return table.concat(bin)
end

local function parsePacket(binStr, idx)
    local function bitsToNum(start, length)
        local num = 0
        for i = start, start + length - 1 do
            num = num * 2 + tonumber(binStr:sub(i,i))
        end
        return num, start + length
    end

    local version, newIdx = bitsToNum(idx, 3)
    local typeID, newIdx = bitsToNum(newIdx, 3)

    if typeID == 4 then
        local value = 0
        while true do
            local group = binStr:sub(newIdx, newIdx)
            local num, nextIdx = bitsToNum(newIdx + 1, 4)
            value = value * 16 + num
            newIdx = nextIdx
            if group == '0' then break end
        end
        return version, newIdx, value
    end

    local lengthTypeID = tonumber(binStr:sub(newIdx, newIdx))
    newIdx = newIdx + 1

    local values = {}
    local subPacketLength, numSubPackets = 0, 0

    if lengthTypeID == 0 then
        subPacketLength, newIdx = bitsToNum(newIdx, 15)
    else
        numSubPackets, newIdx = bitsToNum(newIdx, 11)
    end

    local startIdx = newIdx
    while true do
        if (lengthTypeID == 0 and subPacketLength == 0) or 
           (lengthTypeID == 1 and numSubPackets == 0) then
            break
        end

        local _, nextIdx, subValue = parsePacket(binStr, newIdx)
        table.insert(values, subValue)

        if lengthTypeID == 0 then
            subPacketLength = subPacketLength - (nextIdx - newIdx)
        else
            numSubPackets = numSubPackets - 1
        end
        newIdx = nextIdx
    end

    local result
    if typeID == 0 then
        result = 0
        for _, v in ipairs(values) do result = result + v end
    elseif typeID == 1 then
        result = 1
        for _, v in ipairs(values) do result = result * v end
    elseif typeID == 2 then
        result = math.min(table.unpack(values))
    elseif typeID == 3 then
        result = math.max(table.unpack(values))
    elseif typeID == 5 then
        result = values[1] > values[2] and 1 or 0
    elseif typeID == 6 then
        result = values[1] < values[2] and 1 or 0
    elseif typeID == 7 then
        result = values[1] == values[2] and 1 or 0
    else
        error("Unknown typeID")
    end

    return version, newIdx, result
end

local file = io.open("input.txt", "r")
local hexStr = file:read("*all"):gsub("%s", "")
file:close()

local binStr = hexToBin(hexStr)
local _, _, value = parsePacket(binStr, 1)
print(value)
