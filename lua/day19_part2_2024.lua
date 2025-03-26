
local function split(str, sep)
    local result = {}
    local pattern = "([^" .. sep .. "]+)"
    for part in string.gmatch(str, pattern) do
        table.insert(result, part)
    end
    return result
end

local function strip(str)
    return string.match(str, "^%s*(.-)%s*$")
end

local function count_ways(design, patterns)
    local n = #design
    local dp = {}
    for i = 0, n do
        dp[i] = 0
    end
    dp[0] = 1

    for i = 1, n do
        for _, p in ipairs(patterns) do
            local lp = #p
            if i >= lp then
                if string.sub(design, i - lp + 1, i) == p then
                    dp[i] = dp[i] + dp[i - lp]
                end
            end
        end
    end
    return dp[n]
end

local function main()
    local lines_iterator = io.lines("input.txt")
    if not lines_iterator then
        io.stderr:write("Error: Could not open input.txt\n")
        return
    end

    local patterns_line = lines_iterator()
    if not patterns_line then
         io.stderr:write("Error: Input file is empty or could not read first line\n")
         return
    end

    local available_patterns_raw = split(strip(patterns_line), ",")
    local available_patterns = {}
    for _, p_raw in ipairs(available_patterns_raw) do
        table.insert(available_patterns, strip(p_raw))
    end

    lines_iterator() -- Skip the second line

    local total_ways = 0
    for design in lines_iterator do
         design = strip(design)
         if #design > 0 then
             total_ways = total_ways + count_ways(design, available_patterns)
         end
    end

    print(total_ways)
end

main()
