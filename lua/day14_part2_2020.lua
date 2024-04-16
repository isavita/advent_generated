function generateAddresses(mask, address)
    local floating = {}
    local addresses = {}
    
    -- Apply mask '1's and collect floating positions
    for i = 1, #mask do
        local bit = mask:sub(i, i)
        if bit == '1' then
            address = address | (1 << (35 - (i - 1)))
        elseif bit == 'X' then
            table.insert(floating, 35 - (i - 1))
        end
    end

    -- Generate all combinations for the floating bits
    local count = 1 << #floating
    for i = 0, count - 1 do
        local modAddress = address
        for j, pos in ipairs(floating) do
            if i & (1 << (j - 1)) == 0 then
                modAddress = modAddress & ~(1 << pos) -- Clear bit
            else
                modAddress = modAddress | (1 << pos)  -- Set bit
            end
        end
        table.insert(addresses, modAddress)
    end
    return addresses
end

function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening file")
        return
    end

    local mask = ""
    local mem = {}
    local reMem = "mem%[(%d+)%] = (%d+)"
    
    for line in file:lines() do
        if line:find("mask = ") then
            mask = line:sub(8)
        else
            local address, value = line:match(reMem)
            if address and value then
                address = tonumber(address)
                value = tonumber(value)
                local addresses = generateAddresses(mask, address)
                for _, addr in ipairs(addresses) do
                    mem[addr] = value
                end
            end
        end
    end
    file:close()

    local sum = 0
    for _, value in pairs(mem) do
        sum = sum + value
    end

    print(sum)
end

main()