local function get_directory_sizes(input)
    local current_path = {}
    local directory_sizes = {}

    for line in input:gmatch("[^\r\n]+") do
        local parts = {line:match("^(%S+)%s+(%S+)%s*(.*)$")}
        if parts[1] == "$" then
            if parts[2] == "cd" then
                if parts[3] == "/" then
                    current_path = {}
                elseif parts[3] == ".." then
                    table.remove(current_path)
                else
                    table.insert(current_path, parts[3])
                end
            end
        elseif parts[1] ~= "dir" then
            local size = tonumber(parts[1])
            for i = 1, #current_path do
                local path = table.concat(current_path, "/", 1, i)
                directory_sizes[path] = (directory_sizes[path] or 0) + size
            end
        end
    end

    return directory_sizes
end

local input = io.open("input.txt", "r"):read("*a")
local directory_sizes = get_directory_sizes(input)

local total_size = 0
for _, size in pairs(directory_sizes) do
    if size <= 100000 then
        total_size = total_size + size
    end
end

print(total_size)