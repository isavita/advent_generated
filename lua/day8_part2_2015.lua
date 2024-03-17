local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local total_diff = 0
for line in file:lines() do
    local original_length = #line
    local encoded_length = #(string.format("%q", line))
    total_diff = total_diff + (encoded_length - original_length)
end

file:close()
print(total_diff)