local function parseInput(filename)
    local deps = {}
    local allSteps = {}

    for line in io.lines(filename) do
        local a, b = line:match("Step (%w) must be finished before step (%w) can begin.")
        if a and b then
            deps[b] = (deps[b] or {})
            table.insert(deps[b], a)
            if not allSteps[a] then
                allSteps[a] = {id = a, duration = string.byte(a) - string.byte('A') + 61}
            end
            if not allSteps[b] then
                allSteps[b] = {id = b, duration = string.byte(b) - string.byte('A') + 61}
            end
        end
    end

    return deps, allSteps
end

local function isBeingWorkedOn(step, tasks)
    for _, task in ipairs(tasks) do
        if task == step then
            return true
        end
    end
    return false
end

local function findMinDuration(durations)
    local min = math.huge
    for _, duration in ipairs(durations) do
        if duration > 0 and duration < min then
            min = duration
        end
    end
    return min
end

local function remove(slice, val)
    for i, item in ipairs(slice) do
        if item == val then
            table.remove(slice, i)
            return slice
        end
    end
    return slice
end

local function finishStep(deps, allSteps, step)
    allSteps[step] = nil
    for s, d in pairs(deps) do
        deps[s] = remove(d, step)
    end
end

local function simulateWork(deps, allSteps, numWorkers, baseDuration)
    local workers = {}
    local tasks = {}
    local time = 0

    for i = 1, numWorkers do
        workers[i] = 0
        tasks[i] = 0
    end

    while next(allSteps) do
        local available = {}
        for step, _ in pairs(allSteps) do
            if (not deps[step] or #deps[step] == 0) and not isBeingWorkedOn(step, tasks) then
                table.insert(available, step)
            end
        end
        table.sort(available)

        for i = 1, numWorkers do
            if workers[i] == 0 and #available > 0 then
                tasks[i] = available[1]
                workers[i] = allSteps[available[1]].duration
                table.remove(available, 1)
            end
        end

        local minDuration = findMinDuration(workers)
        for i = 1, numWorkers do
            if workers[i] ~= 0 then
                workers[i] = workers[i] - minDuration
                if workers[i] == 0 then
                    finishStep(deps, allSteps, tasks[i])
                    tasks[i] = 0
                end
            end
        end
        time = time + minDuration
    end

    return time
end

local deps, allSteps = parseInput("input.txt")
local timeTaken = simulateWork(deps, allSteps, 5, 60)
print(timeTaken)