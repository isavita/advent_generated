
cubes = {}

for line in io.lines("input.txt") do
    local coords = {}
    for coord in string.gmatch(line, "%d+") do
        table.insert(coords, tonumber(coord))
    end
    cubes[#cubes + 1] = {x = coords[1], y = coords[2], z = coords[3]}
end

function calculateExposedSides(p, cubes)
    directions = {
        {1, 0, 0}, {-1, 0, 0}, -- x directions
        {0, 1, 0}, {0, -1, 0}, -- y directions
        {0, 0, 1}, {0, 0, -1}  -- z directions
    }

    exposedSides = 6
    for _, dir in ipairs(directions) do
        adjacent = {x = p.x + dir[1], y = p.y + dir[2], z = p.z + dir[3]}
        for _, cube in ipairs(cubes) do
            if cube.x == adjacent.x and cube.y == adjacent.y and cube.z == adjacent.z then
                exposedSides = exposedSides - 1
                break
            end
        end
    end
    return exposedSides
end

surfaceArea = 0
for i = 1, #cubes do
    surfaceArea = surfaceArea + calculateExposedSides(cubes[i], cubes)
end

print(surfaceArea)
