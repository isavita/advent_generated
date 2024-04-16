local function read_file(filename)
    local file = io.open(filename, "r")
    if not file then
        error("File not found")
    end
    local content = file:read("*a")
    file:close()
    return content
end

local function split_string(str, delimiter)
    local result = {}
    for match in (str..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match)
    end
    return result
end

local function main()
    local content = read_file("input.txt")
    local lines = split_string(content, "\n")
    
    local root = {""}
    local dirs = {}
    local files = {}
    local curr = root
    
    for _, line in ipairs(lines) do
        local txt = split_string(line, " ")
        if txt[1] == "$" then
            if txt[2] == "cd" then
                if txt[3] == "/" then
                    curr = root
                elseif txt[3] == ".." then
                    table.remove(curr)
                else
                    table.insert(curr, txt[3])
                end
                dirs[table.concat(curr, "/")] = 0
            end
        else
            if txt[1] ~= "dir" then
                local path = table.concat(curr, "/") .. "/" .. txt[2]
                files[path] = tonumber(txt[1])
            end
        end
    end
    
    for f, s in pairs(files) do
        local path = split_string(f, "/")
        for i = 1, #path do
            local dir = table.concat(path, "/", 1, i)
            dirs[dir] = (dirs[dir] or 0) + s
        end
    end
    
    local sorted_sizes = {}
    for _, s in pairs(dirs) do
        table.insert(sorted_sizes, s)
    end
    
    table.sort(sorted_sizes)
    local total = 70000000
    local want = 30000000
    local available = total - (dirs[""] or 0)
    
    local function search(target)
        local low, high = 1, #sorted_sizes
        while low <= high do
            local mid = math.floor((low + high) / 2)
            if sorted_sizes[mid] >= target then
                high = mid - 1
            else
                low = mid + 1
            end
        end
        return low
    end
    
    print(sorted_sizes[search(want - available)])
end

main()