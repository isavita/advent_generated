
struct RebootStep
    action::String
    xStart::Int
    xEnd::Int
    yStart::Int
    yEnd::Int
    zStart::Int
    zEnd::Int
end

function parseRebootStep(line::String)
    parts = split(line, " ")

    action = parts[1]
    parts = split(parts[2], ",")
    xRange = split(parts[1][3:end], "..")
    yRange = split(parts[2][3:end], "..")
    zRange = split(parts[3][3:end], "..")

    xStart = parse(Int, xRange[1])
    xEnd = parse(Int, xRange[2])
    yStart = parse(Int, yRange[1])
    yEnd = parse(Int, yRange[2])
    zStart = parse(Int, zRange[1])
    zEnd = parse(Int, zRange[2])

    return RebootStep(action, xStart, xEnd, yStart, yEnd, zStart, zEnd)
end

function createCubeGrid(minCoord::Int, maxCoord::Int)
    gridSize = maxCoord - minCoord + 1
    grid = falses(gridSize, gridSize, gridSize)

    return grid
end

function executeRebootSteps(cubeGrid, rebootSteps)
    for step in rebootSteps
        if !(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50)
            continue
        end
        for x = step.xStart:step.xEnd, y = step.yStart:step.yEnd, z = step.zStart:step.zEnd
            cubeGrid[x+51, y+51, z+51] = step.action == "on"
        end
    end
end

function countOnCubes(cubeGrid)
    count = 0

    for i = 1:size(cubeGrid, 1), j = 1:size(cubeGrid, 2), k = 1:size(cubeGrid, 3)
        if cubeGrid[i, j, k]
            count += 1
        end
    end

    return count
end

file = open("input.txt")
rebootSteps = []

for line in eachline(file)
    if line == ""
        continue
    end
    step = parseRebootStep(line)
    push!(rebootSteps, step)
end

minCoord, maxCoord = -50, 50
cubeGrid = createCubeGrid(minCoord, maxCoord)
executeRebootSteps(cubeGrid, rebootSteps)
onCubes = countOnCubes(cubeGrid)

println(onCubes)
