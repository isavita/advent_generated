-- Read input from file
local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

-- Parse input
local entries = {}
for line in input:gmatch("[^\n]+") do
    local timestamp, event = line:match("%[([^%]]+)%] (.+)")
    local year, month, day, hour, minute = timestamp:match("(%d+)-(%d+)-(%d+) (%d+):(%d+)")
    local guard_id = event:match("#(%d+)")
    local action = event:match("falls asleep") and "falls_asleep" or event:match("wakes up") and "wakes_up" or "begins_shift"
    table.insert(entries, {
        year = tonumber(year),
        month = tonumber(month),
        day = tonumber(day),
        hour = tonumber(hour),
        minute = tonumber(minute),
        guard_id = tonumber(guard_id),
        action = action
    })
end

-- Sort entries by timestamp
table.sort(entries, function(a, b)
    if a.year ~= b.year then return a.year < b.year end
    if a.month ~= b.month then return a.month < b.month end
    if a.day ~= b.day then return a.day < b.day end
    if a.hour ~= b.hour then return a.hour < b.hour end
    return a.minute < b.minute
end)

-- Calculate total sleep time for each guard
local guard_sleep_times = {}
local current_guard = nil
local sleep_start = nil
for _, entry in ipairs(entries) do
    if entry.action == "begins_shift" then
        current_guard = entry.guard_id
        guard_sleep_times[current_guard] = guard_sleep_times[current_guard] or 0
    elseif entry.action == "falls_asleep" then
        sleep_start = entry.minute
    elseif entry.action == "wakes_up" then
        guard_sleep_times[current_guard] = guard_sleep_times[current_guard] + (entry.minute - sleep_start)
    end
end

-- Find the guard with the most total sleep time
local max_sleep_time = 0
local max_sleep_guard = nil
for guard_id, sleep_time in pairs(guard_sleep_times) do
    if sleep_time > max_sleep_time then
        max_sleep_time = sleep_time
        max_sleep_guard = guard_id
    end
end

-- Find the minute the max sleep guard was asleep the most
local minute_counts = {}
current_guard = nil
sleep_start = nil
for _, entry in ipairs(entries) do
    if entry.action == "begins_shift" then
        current_guard = entry.guard_id
    elseif entry.action == "falls_asleep" then
        sleep_start = entry.minute
    elseif entry.action == "wakes_up" then
        for i = sleep_start, entry.minute - 1 do
            minute_counts[i] = (minute_counts[i] or 0) + (current_guard == max_sleep_guard and 1 or 0)
        end
    end
end

local max_minute = 0
local max_minute_count = 0
for minute, count in pairs(minute_counts) do
    if count > max_minute_count then
        max_minute = minute
        max_minute_count = count
    end
end

-- Part 1 answer
print(max_sleep_guard * max_minute)

-- Part 2 answer
local max_minute_guard = nil
local max_minute_count = 0
for guard_id, sleep_times in pairs(guard_sleep_times) do
    minute_counts = {}
    current_guard = nil
    sleep_start = nil
    for _, entry in ipairs(entries) do
        if entry.action == "begins_shift" then
            current_guard = entry.guard_id
        elseif entry.action == "falls_asleep" then
            sleep_start = entry.minute
        elseif entry.action == "wakes_up" then
            for i = sleep_start, entry.minute - 1 do
                minute_counts[i] = (minute_counts[i] or 0) + (current_guard == guard_id and 1 or 0)
            end
        end
    end
    for minute, count in pairs(minute_counts) do
        if count > max_minute_count then
            max_minute_guard = guard_id
            max_minute = minute
            max_minute_count = count
        end
    end
end

print(max_minute_guard * max_minute)