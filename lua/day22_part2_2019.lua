
--[[
Lua solution for Advent of Code 2019 Day 22: Slam Shuffle

Handles both Part 1 and Part 2.
Reads shuffle instructions from input.txt.
Prints the results to standard output.

Requires Lua 5.3+ for proper 64-bit integer arithmetic.
The large numbers in Part 2 necessitate modular arithmetic functions
that avoid overflow even during intermediate calculations.
]]

-- Helper functions for modular arithmetic with large numbers

-- Modular multiplication: (a * b) % m
-- Avoids overflow by using the Russian Peasant Multiplication algorithm (binary method)
local function modMul(a, b, m)
    -- Reduce a and b modulo m first
    a = a % m
    b = b % m
    if a < 0 then a = a + m end
    if b < 0 then b = b + m end

    local res = 0
    a = a % m -- Ensure a is already reduced

    while b > 0 do
        if b % 2 == 1 then
            res = (res + a) % m
        end
        a = (a * 2) % m
        b = math.floor(b / 2) -- Integer division
    end
    return res
end

-- Modular exponentiation: (base^exp) % m
-- Uses exponentiation by squaring and modMul
local function modPow(base, exp, m)
    if m == 1 then return 0 end
    local res = 1
    base = base % m
    if base < 0 then base = base + m end

    while exp > 0 do
        if exp % 2 == 1 then
            res = modMul(res, base, m)
        end
        base = modMul(base, base, m)
        exp = math.floor(exp / 2) -- Integer division
    end
    return res
end

-- Extended Euclidean Algorithm: finds g, x, y such that ax + by = g = gcd(a, b)
local function gcdExtended(a, b)
    if a == 0 then
        return b, 0, 1
    end
    local g, x1, y1 = gcdExtended(b % a, a)
    local x = y1 - math.floor(b / a) * x1
    local y = x1
    return g, x, y
end

-- Modular multiplicative inverse: finds x such that (a * x) % m == 1
-- Requires gcd(a, m) == 1
local function modInverse(a, m)
    local g, x, y = gcdExtended(a, m)
    if g ~= 1 then
        -- Inverse doesn't exist
        error("Modular inverse does not exist for a=" .. tostring(a) .. ", m=" .. tostring(m))
    end
    -- Ensure x is positive
    return (x % m + m) % m
end


-- Parses a shuffle instruction line
local function parseInstruction(line)
    if line == "deal into new stack" then
        return { type = "deal_new" }
    elseif line:match("^cut (.+)$") then
        local n = tonumber(line:match("^cut (.+)$"))
        return { type = "cut", n = n }
    elseif line:match("^deal with increment (.+)$") then
        local n = tonumber(line:match("^deal with increment (.+)$"))
        return { type = "deal_inc", n = n }
    else
        error("Unknown instruction: " .. line)
    end
end

-- Part 1: Track the position of a specific card
local function solvePart1(instructions, deckSize, cardToTrack)
    local pos = cardToTrack

    for _, instr in ipairs(instructions) do
        if instr.type == "deal_new" then
            -- pos' = N - 1 - pos
            pos = deckSize - 1 - pos
        elseif instr.type == "cut" then
            -- pos' = (pos - n) % N
            pos = (pos - instr.n) % deckSize
        elseif instr.type == "deal_inc" then
            -- pos' = (pos * n) % N
            -- Need modMul here in case pos * n exceeds max safe integer before modulo,
            -- although for Part 1 deck size, standard multiplication is likely fine.
            -- Using modMul for consistency and safety.
            pos = modMul(pos, instr.n, deckSize)
        end
        -- Ensure position remains positive
        if pos < 0 then pos = pos + deckSize end
    end
    return pos
end

-- Part 2: Determine the card at a specific position after many shuffles
-- Uses linear congruential transformation and modular arithmetic
local function solvePart2(instructions, deckSize, shuffleCount, targetPos)
    -- The shuffling process applies a linear transformation:
    -- f(x) = (a*x + b) % N
    -- We need to find 'a' and 'b' for the entire sequence of instructions.
    -- Start with the identity transformation: a=1, b=0
    local a = 1
    local b = 0

    -- Calculate the combined (a, b) for one full shuffle sequence
    for _, instr in ipairs(instructions) do
        if instr.type == "deal_new" then
            -- f'(x) = N - 1 - f(x) = N - 1 - (ax + b) = (-a)x + (N - 1 - b)
            a = -a
            b = deckSize - 1 - b
        elseif instr.type == "cut" then
            -- f'(x) = f(x) - n = (ax + b) - n = ax + (b - n)
            b = b - instr.n
        elseif instr.type == "deal_inc" then
            -- f'(x) = f(x) * n = (ax + b) * n = (a*n)x + (b*n)
            a = modMul(a, instr.n, deckSize)
            b = modMul(b, instr.n, deckSize)
        end
        -- Keep a and b within [0, N-1]
        a = a % deckSize
        b = b % deckSize
        if a < 0 then a = a + deckSize end
        if b < 0 then b = b + deckSize end
    end

    -- We have the transformation for one shuffle: f(x) = (a*x + b) % N
    -- We want to find the card number 'x' that ends up at targetPos after M shuffles.
    -- Let the final position be y. y = f^M(x).
    -- We need to find x given y = targetPos. This requires inverting the transformation.
    -- The inverse transformation g(y) finds the original position x from the final position y.
    -- y = ax + b  =>  ax = y - b  =>  x = a_inv * (y - b) = a_inv * y - a_inv * b
    -- So, g(y) = (a' * y + b') % N, where a' = modInverse(a, N) and b' = (-modInverse(a, N) * b) % N

    -- However, applying the inverse M times is equivalent to finding the parameters
    -- for f^M and then solving for x.
    -- f^M(x) = (a^M * x + b * (a^M - 1) * modInverse(a - 1, N)) % N
    -- Let A = a^M % N
    -- Let B = (b * (a^M - 1) * modInverse(a - 1, N)) % N
    -- Note: The geometric series sum (1 + a + ... + a^(M-1)) only works if a != 1.
    -- If a == 1, then f(x) = x + b, and f^M(x) = x + M*b.

    local A, B
    if a == 1 then
        -- f^M(x) = x + M*b mod N
        A = 1
        B = modMul(shuffleCount, b, deckSize)
    else
        -- A = a^M mod N
        A = modPow(a, shuffleCount, deckSize)

        -- B = b * (a^M - 1) * modInverse(a - 1, N) mod N
        local term1 = (A - 1 + deckSize) % deckSize -- (a^M - 1) mod N, ensure positive
        local term2 = modInverse((a - 1 + deckSize) % deckSize, deckSize) -- modInverse(a - 1, N)
        
        B = modMul(b, modMul(term1, term2, deckSize), deckSize)
    end
    
    -- We have targetPos = (A * x + B) % N
    -- We need to solve for x (the original card number).
    -- A * x = (targetPos - B) % N
    -- x = (targetPos - B) * modInverse(A, N) % N

    local term3 = (targetPos - B + deckSize) % deckSize -- (targetPos - B) mod N, ensure positive
    local A_inv = modInverse(A, deckSize)

    local originalCard = modMul(term3, A_inv, deckSize)

    return originalCard
end

-- Main execution block
local function main()
    local input_file = "input.txt"
    local file = io.open(input_file, "r")
    if not file then
        print("Error: Could not open " .. input_file)
        return
    end

    local instructions = {}
    for line in file:lines() do
        table.insert(instructions, parseInstruction(line))
    end
    file:close()

    -- Part 1 constants
    local DECK_SIZE_1 = 10007
    local CARD_TO_TRACK = 2019

    -- Part 2 constants
    -- Using Lua 5.3+ integer literals (no suffix needed)
    local DECK_SIZE_2 = 119315717514047
    local SHUFFLE_COUNT = 101741582076661
    local TARGET_POS = 2020

    -- Calculate and print results
    local result1 = solvePart1(instructions, DECK_SIZE_1, CARD_TO_TRACK)
    print("Part 1: Position of card " .. CARD_TO_TRACK .. " is " .. result1)

    local result2 = solvePart2(instructions, DECK_SIZE_2, SHUFFLE_COUNT, TARGET_POS)
    print("Part 2: Card at position " .. TARGET_POS .. " is " .. result2)

end

-- Run the main function
main()

