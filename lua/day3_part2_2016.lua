-- Read input from file
local file = io.open("input.txt", "r")
local numbers = {}
for line in file:lines() do
    local row = {}
    for num in line:gmatch("%d+") do
        table.insert(row, tonumber(num))
    end
    table.insert(numbers, row)
end
file:close()

-- Function to check if three sides form a valid triangle
local function is_valid_triangle(a, b, c)
    return a + b > c and a + c > b and b + c > a
end

-- Count valid triangles
local valid_triangles = 0
for i = 1, #numbers[1] do
    for j = 1, #numbers, 3 do
        if j + 2 <= #numbers and is_valid_triangle(numbers[j][i], numbers[j+1][i], numbers[j+2][i]) then
            valid_triangles = valid_triangles + 1
        end
    end
end

-- Print the answer
print(valid_triangles)