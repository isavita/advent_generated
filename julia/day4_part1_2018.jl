using Dates

struct Record
    timestamp::DateTime
    action::String
    guardID::Int
end

function read_and_parse_input(filename::String)::Vector{Record}
    records = Record[]
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, "] ")
            time_part = parts[1][2:end]
            action_part = parts[2]
            timestamp = DateTime(time_part, "yyyy-mm-dd HH:MM")
            guard_id = -1
            if occursin("Guard", action_part)
                guard_id = parse(Int, match(r"#(\d+)", action_part).captures[1])
                action_part = "begins shift"
            elseif occursin("falls asleep", action_part)
                action_part = "falls asleep"
            elseif occursin("wakes up", action_part)
                action_part = "wakes up"
            end
            push!(records, Record(timestamp, action_part, guard_id))
        end
    end
    sort!(records, by=x->x.timestamp)
    return records
end

function main()
    records = read_and_parse_input("input.txt")
    guard_sleep_minutes = Dict{Int,Vector{Int}}()
    current_guard_id = -1
    sleep_start = DateTime(0)

    for record in records
        if record.action == "begins shift"
            current_guard_id = record.guardID
        elseif record.action == "falls asleep"
            sleep_start = record.timestamp
        elseif record.action == "wakes up"
            if !haskey(guard_sleep_minutes, current_guard_id)
                guard_sleep_minutes[current_guard_id] = zeros(Int, 60)
            end
            for i in Dates.minute(sleep_start):Dates.minute(record.timestamp)-1
                guard_sleep_minutes[current_guard_id][i+1] += 1
            end
        end
    end

    max_sleep = 0
    sleepiest_guard = -1
    for (guard_id, minutes) in guard_sleep_minutes
        total_sleep = sum(minutes)
        if total_sleep > max_sleep
            max_sleep = total_sleep
            sleepiest_guard = guard_id
        end
    end

    max_minute = 0
    max_minute_count = 0
    for (i, count) in enumerate(guard_sleep_minutes[sleepiest_guard])
        if count > max_minute_count
            max_minute_count = count
            max_minute = i - 1
        end
    end

    println(sleepiest_guard * max_minute)
end

main()