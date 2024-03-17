local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local total_diff = 0

for line in file:lines() do
    local code_length = #line
    local memory_length = 0
    local in_escape = false
    local hex_count = 0

    for i = 2, #line - 1 do
        if hex_count > 0 then
            hex_count = hex_count - 1
        elseif in_escape then
            if line:sub(i, i) == "x" then
                hex_count = 2
            end
            in_escape = false
            memory_length = memory_length + 1
        elseif line:sub(i, i) == "\\" then
            in_escape = true
        else
            memory_length = memory_length + 1
        end
    end

    total_diff = total_diff + code_length - memory_length
end

file:close()
print(total_diff)