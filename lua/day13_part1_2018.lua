local function readInput(filename)
    local map = {}
    local carts = {}
    local cartSymbols = {['^'] = true, ['v'] = true, ['<'] = true, ['>'] = true}
    local cartDirections = {['^'] = {0, -1}, ['v'] = {0, 1}, ['<'] = {-1, 0}, ['>'] = {1, 0}}
    local cartTurns = {['^'] = '|', ['v'] = '|', ['<'] = '-', ['>'] = '-'}

    local file = io.open(filename, "r")
    local y = 0
    for line in file:lines() do
        map[y] = {}
        for x = 1, #line do
            local c = line:sub(x, x)
            map[y][x - 1] = c
            if cartSymbols[c] then
                table.insert(carts, {x = x - 1, y = y, dir = c, turns = 0})
                map[y][x - 1] = cartTurns[c]
            end
        end
        y = y + 1
    end
    file:close()
    return map, carts
end

local function simulate(map, carts)
    local directions = {['^'] = {0, -1}, ['v'] = {0, 1}, ['<'] = {-1, 0}, ['>'] = {1, 0}}
    local leftTurn = {['^'] = '<', ['v'] = '>', ['<'] = 'v', ['>'] = '^'}
    local rightTurn = {['^'] = '>', ['v'] = '<', ['<'] = '^', ['>'] = 'v'}
    local sortCarts = function(a, b)
        if a.y == b.y then
            return a.x < b.x
        else
            return a.y < b.y
        end
    end

    while true do
        table.sort(carts, sortCarts)
        local positions = {}
        for _, cart in ipairs(carts) do
            positions[cart.y * 1000 + cart.x] = nil -- Clear old position
            cart.x = cart.x + directions[cart.dir][1]
            cart.y = cart.y + directions[cart.dir][2]
            local newPos = cart.y * 1000 + cart.x
            if positions[newPos] then
                return cart.x, cart.y -- Collision detected
            end
            positions[newPos] = true

            local newChar = map[cart.y][cart.x]
            if newChar == '+' then
                if cart.turns % 3 == 0 then
                    cart.dir = leftTurn[cart.dir]
                elseif cart.turns % 3 == 2 then
                    cart.dir = rightTurn[cart.dir]
                end
                cart.turns = cart.turns + 1
            elseif newChar == '/' then
                if cart.dir == '^' or cart.dir == 'v' then
                    cart.dir = rightTurn[cart.dir]
                else
                    cart.dir = leftTurn[cart.dir]
                end
            elseif newChar == '\\' then
                if cart.dir == '^' or cart.dir == 'v' then
                    cart.dir = leftTurn[cart.dir]
                else
                    cart.dir = rightTurn[cart.dir]
                end
            end
        end
    end
end

local map, carts = readInput("input.txt")
local x, y = simulate(map, carts)
print("First crash occurs at: " .. x .. "," .. y)