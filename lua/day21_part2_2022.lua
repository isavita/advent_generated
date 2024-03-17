local Monkey = {}
Monkey.__index = Monkey

function Monkey:new(name)
    local self = setmetatable({
        name = name,
        val = 0,
        has_val = false,
        left = nil,
        right = nil,
        op = nil
    }, Monkey)
    return self
end

function Monkey:solve()
    if self.has_val then
        return self.val, true
    end

    if self.left ~= nil and self.right ~= nil then
        local left, l_ok = self.left:solve()
        local right, r_ok = self.right:solve()

        if l_ok and r_ok then
            if self.op == "+" then
                return left + right, true
            elseif self.op == "-" then
                return left - right, true
            elseif self.op == "*" then
                return left * right, true
            elseif self.op == "/" then
                return left / right, true
            elseif self.op == "==" then
                if left == right then
                    return 0, true
                else
                    return 1, true
                end
            end
        end
    end
    return 0, false
end

function Monkey:expect(x)
    if self.name == "humn" then
        return x
    end

    local left, l_ok = self.left:solve()
    local right, r_ok = self.right:solve()

    if not l_ok then
        if self.op == "+" then
            return self.left:expect(x - right)
        elseif self.op == "-" then
            return self.left:expect(x + right)
        elseif self.op == "*" then
            return self.left:expect(x / right)
        elseif self.op == "/" then
            return self.left:expect(x * right)
        elseif self.op == "==" then
            return self.left:expect(right)
        end
    end

    if not r_ok then
        if self.op == "+" then
            return self.right:expect(x - left)
        elseif self.op == "-" then
            return self.right:expect(left - x)
        elseif self.op == "*" then
            return self.right:expect(x / left)
        elseif self.op == "/" then
            return self.right:expect(left / x)
        elseif self.op == "==" then
            return self.right:expect(left)
        end
    end

    error("impossible")
end

local function parse()
    local index = {}

    local function init_monkey(s)
        if not index[s] then
            index[s] = Monkey:new(s)
        end
    end

    local file = io.open("input.txt", "r")
    if not file then
        error("Failed to open input.txt")
    end

    for line in file:lines() do
        local goal, rest = line:match("([^:]+): (.*)")
        init_monkey(goal)

        local num = tonumber(rest)
        if num then
            index[goal].val = num
            index[goal].has_val = true
        else
            local left, op, right = rest:match("([^ ]+) ([^ ]+) ([^ ]+)")
            init_monkey(left)
            init_monkey(right)
            index[goal].left = index[left]
            index[goal].op = op
            index[goal].right = index[right]
        end
    end

    file:close()
    return index
end

local index = parse()

-- Set "humn" as unknown
index["humn"].has_val = false

-- New operator: == returns 0 if equals, 1 otherwise
index["root"].op = "=="

print(index["root"]:expect(0))