local file = io.open("input.txt", "r")
local line = file:read("*l")
local fish_ages = {}
for age in line:gmatch("%d+") do
    table.insert(fish_ages, tonumber(age))
end
file:close()

local lantern_fish_counts = {0, 0, 0, 0, 0, 0, 0, 0, 0}
for _, age in ipairs(fish_ages) do
    lantern_fish_counts[age + 1] = lantern_fish_counts[age + 1] + 1
end

for _ = 1, 256 do
    local new_lantern_fish = lantern_fish_counts[1]
    for i = 1, 8 do
        lantern_fish_counts[i] = lantern_fish_counts[i + 1]
    end
    lantern_fish_counts[7] = lantern_fish_counts[7] + new_lantern_fish
    lantern_fish_counts[9] = new_lantern_fish
end

local total = 0
for _, count in ipairs(lantern_fish_counts) do
    total = total + count
end
print(total)