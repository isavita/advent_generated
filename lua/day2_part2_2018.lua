
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

for i = 1, #lines - 1 do
    for j = i + 1, #lines do
        local diff = 0
        for k = 1, #lines[i] do
            if lines[i]:sub(k, k) ~= lines[j]:sub(k, k) then
                diff = diff + 1
                if diff > 1 then
                    break
                end
            end
        end
        if diff == 1 then
            local common = ""
            for k = 1, #lines[i] do
                if lines[i]:sub(k, k) == lines[j]:sub(k, k) then
                    common = common .. lines[i]:sub(k, k)
                end
            end
            print(common)
            return
        end
    end
end
