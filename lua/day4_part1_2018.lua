local function parseTimestamp(dateStr)
    local year, month, day, hour, minute = dateStr:match("(%d+)-(%d+)-(%d+) (%d+):(%d+)")
    return {year = year, month = month, day = day, hour = hour, minute = minute}
end

local function readAndParseInput(filename)
    local records = {}
    for line in io.lines(filename) do
        local timePart, actionPart = line:match("%[(.-)%] (.+)")
        local ts = parseTimestamp(timePart)

        local guardID = -1
        if actionPart:find("Guard") then
            guardID = actionPart:match("#(%d+)")
            actionPart = "begins shift"
        elseif actionPart:find("falls asleep") then
            actionPart = "falls asleep"
        elseif actionPart:find("wakes up") then
            actionPart = "wakes up"
        end

        table.insert(records, {timestamp = ts, action = actionPart, guardID = tonumber(guardID)})
    end
    return records
end

local function compareTimestamps(a, b)
    if a.year ~= b.year then return a.year < b.year end
    if a.month ~= b.month then return a.month < b.month end
    if a.day ~= b.day then return a.day < b.day end
    if a.hour ~= b.hour then return a.hour < b.hour end
    return a.minute < b.minute
end

local records = readAndParseInput("input.txt")
table.sort(records, function(a, b)
    return compareTimestamps(a.timestamp, b.timestamp)
end)

local guardSleepMinutes = {}
local currentGuardID = nil
local sleepStart = nil

for _, record in ipairs(records) do
    if record.action == "begins shift" then
        currentGuardID = record.guardID
    elseif record.action == "falls asleep" then
        sleepStart = record.timestamp
    elseif record.action == "wakes up" then
        guardSleepMinutes[currentGuardID] = guardSleepMinutes[currentGuardID] or {}
        for i = tonumber(sleepStart.minute), tonumber(record.timestamp.minute) - 1 do
            guardSleepMinutes[currentGuardID][i] = (guardSleepMinutes[currentGuardID][i] or 0) + 1
        end
    end
end

local maxSleep = 0
local sleepiestGuard = nil
for guardID, minutes in pairs(guardSleepMinutes) do
    local totalSleep = 0
    for _, count in pairs(minutes) do
        totalSleep = totalSleep + count
    end
    if totalSleep > maxSleep then
        maxSleep = totalSleep
        sleepiestGuard = guardID
    end
end

local maxMinute = nil
local maxMinuteCount = 0
for i, count in pairs(guardSleepMinutes[sleepiestGuard]) do
    if count > maxMinuteCount then
        maxMinuteCount = count
        maxMinute = i
    end
end

print(sleepiestGuard * maxMinute)