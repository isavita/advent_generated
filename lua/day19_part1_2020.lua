local function parse_rule(line)
    local id, rule = line:match("(%d+): (.+)")
    id = tonumber(id)
    if rule:find('"') then
        return id, rule:sub(2, -2) -- Direct character match, removing quotes
    else
        local subrules = {}
        for part in rule:gmatch("[^|]+") do
            local subrule = {}
            for num in part:gmatch("%d+") do
                table.insert(subrule, tonumber(num))
            end
            table.insert(subrules, subrule)
        end
        return id, subrules
    end
end

local function read_input(filename)
    local rules = {}
    local messages = {}
    local reading_rules = true
    for line in io.lines(filename) do
        if line == "" then
            reading_rules = false
        elseif reading_rules then
            local id, rule = parse_rule(line)
            rules[id] = rule
        else
            table.insert(messages, line)
        end
    end
    return rules, messages
end

local function match_rule(rules, rule_id, message, start)
    local rule = rules[rule_id]
    if type(rule) == "string" then
        return message:sub(start, start) == rule and start + 1
    else
        for _, subrule in ipairs(rule) do
            local pos = start
            local matches = true
            for _, sub in ipairs(subrule) do
                pos = match_rule(rules, sub, message, pos)
                if not pos then
                    matches = false
                    break
                end
            end
            if matches then
                return pos
            end
        end
        return nil
    end
end

local function count_valid_messages(rules, messages)
    local count = 0
    for _, message in ipairs(messages) do
        local match_end = match_rule(rules, 0, message, 1)
        if match_end and match_end - 1 == #message then
            count = count + 1
        end
    end
    return count
end

local rules, messages = read_input("input.txt")
local valid_count = count_valid_messages(rules, messages)
print("Number of valid messages:", valid_count)