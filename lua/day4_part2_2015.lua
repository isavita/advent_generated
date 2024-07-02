-- Simplified MD5 implementation
local function md5(input)
    local function lrotate(x, by)
        return ((x << by) & 0xFFFFFFFF) | (x >> (32 - by))
    end

    local function bytes_to_int32(a, b, c, d)
        return a | (b << 8) | (c << 16) | (d << 24)
    end

    local function int32_to_hex(i)
        return string.format("%02x%02x%02x%02x",
            i & 0xFF,
            (i >> 8) & 0xFF,
            (i >> 16) & 0xFF,
            (i >> 24) & 0xFF)
    end

    local r = {
        7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
        5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
        4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
        6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
    }

    local k = {
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
    }

    local function pad_input(input)
        local len = #input
        local pad_len = 56 - (len % 64)
        if pad_len <= 0 then pad_len = pad_len + 64 end
        return input .. string.char(0x80) .. string.rep("\0", pad_len - 1) .. 
               string.char(((len * 8) & 0xFF),
                           ((len * 8 >> 8) & 0xFF),
                           ((len * 8 >> 16) & 0xFF),
                           ((len * 8 >> 24) & 0xFF),
                           0, 0, 0, 0)
    end

    input = pad_input(input)
    local h0 = 0x67452301
    local h1 = 0xEFCDAB89
    local h2 = 0x98BADCFE
    local h3 = 0x10325476

    for i = 1, #input, 64 do
        local chunk = input:sub(i, i + 63)
        local w = {}
        for j = 1, 16 do
            w[j] = bytes_to_int32(chunk:byte(j*4-3, j*4))
        end

        local a, b, c, d = h0, h1, h2, h3

        for j = 0, 63 do
            local f, g
            if j <= 15 then
                f = (b & c) | ((~b) & d)
                g = j
            elseif j <= 31 then
                f = (d & b) | ((~d) & c)
                g = (5*j + 1) % 16
            elseif j <= 47 then
                f = b ~ c ~ d
                g = (3*j + 5) % 16
            else
                f = c ~ (b | (~d))
                g = (7*j) % 16
            end
            
            local temp = d
            d = c
            c = b
            b = b + lrotate((a + f + k[j+1] + w[g+1]) & 0xFFFFFFFF, r[j+1])
            a = temp
        end

        h0 = (h0 + a) & 0xFFFFFFFF
        h1 = (h1 + b) & 0xFFFFFFFF
        h2 = (h2 + c) & 0xFFFFFFFF
        h3 = (h3 + d) & 0xFFFFFFFF
    end

    return int32_to_hex(h0) .. int32_to_hex(h1) .. int32_to_hex(h2) .. int32_to_hex(h3)
end

-- Function to read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error: Unable to open input.txt")
        os.exit(1)
    end
    local content = file:read("*all")
    file:close()
    return content:gsub("^%s*(.-)%s*$", "%1") -- Trim whitespace
end

-- Function to find the lowest number that produces a hash with five leading zeros
local function find_advent_coin(secret_key)
    local number = 0
    while true do
        local hash = md5(secret_key .. tostring(number))
        if hash:sub(1, 6) == "000000" then
            return number
        end
        number = number + 1
    end
end

-- Main execution
local secret_key = read_input()
local result = find_advent_coin(secret_key)
print(result)
