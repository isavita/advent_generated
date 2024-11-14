using DelimitedFiles

function calculate(monkey, jobs, results)
    if haskey(results, monkey)
        return results[monkey]
    end

    job = jobs[monkey]
    if tryparse(Int, job) !== nothing
        results[monkey] = parse(Int, job)
        return results[monkey]
    end

    parts = split(job)
    a = calculate(parts[1], jobs, results)
    b = calculate(parts[3], jobs, results)

    result = if parts[2] == "+"
        a + b
    elseif parts[2] == "-"
        a - b
    elseif parts[2] == "*"
        a * b
    elseif parts[2] == "/"
        div(a, b)
    else
        error("Unknown operation: ", parts[2])
    end

    results[monkey] = result
    return result
end

jobs = Dict{String,String}()
results = Dict{String,Int}()

open("input.txt", "r") do file
    for line in eachline(file)
        parts = split(line, ": ")
        jobs[parts[1]] = parts[2]
    end
end

println(calculate("root", jobs, results))