using Dates

function parse_record(record)
    time_str = record[2:17]
    action = record[20:end]
    time = DateTime(time_str, "yyyy-mm-dd HH:MM")
    return (time, action)
end

function process_records(records)
    sorted_records = sort(records, by = x -> x[1])
    guards_sleep = Dict{Int, Vector{Int}}()
    current_guard = nothing
    asleep_time = nothing

    for (time, action) in sorted_records
        if occursin("Guard", action)
            guard_id = parse(Int, match(r"\d+", action).match)
            current_guard = guard_id
            if !haskey(guards_sleep, guard_id)
                guards_sleep[guard_id] = zeros(Int, 60)  # 60 minutes
            end
        elseif action == "falls asleep"
            asleep_time = minute(time)
        elseif action == "wakes up"
            wake_time = minute(time)
            for min in asleep_time:wake_time-1
                guards_sleep[current_guard][min+1] += 1
            end
        end
    end
    return guards_sleep
end

function strategy1(guards_sleep)
    max_sleep = -1
    sleepy_guard = nothing
    for (guard, minutes) in guards_sleep
        total_sleep = sum(minutes)
        if total_sleep > max_sleep
            max_sleep = total_sleep
            sleepy_guard = guard
        end
    end
    sleepiest_minute = argmax(guards_sleep[sleepy_guard])
    return sleepy_guard * (sleepiest_minute - 1)
end

function strategy2(guards_sleep)
    max_frequency = -1
    frequent_guard = nothing
    frequent_minute = nothing

    for (guard, minutes) in guards_sleep
        max_minute = maximum(minutes)
        if max_minute > max_frequency
            max_frequency = max_minute
            frequent_guard = guard
            frequent_minute = argmax(minutes)
        end
    end
    return frequent_guard * (frequent_minute - 1)
end

function main()
    records = []
    open("input.txt", "r") do file
        for line in eachline(file)
            push!(records, parse_record(line))
        end
    end

    guards_sleep = process_records(records)
    answer1 = strategy1(guards_sleep)
    answer2 = strategy2(guards_sleep)

    println("Strategy 1 Answer: ", answer1)
    println("Strategy 2 Answer: ", answer2)
end

main()