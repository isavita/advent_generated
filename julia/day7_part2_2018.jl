mutable struct Step
    id::Char
    duration::Int
end

function parse_input(filename)
    deps = Dict{Char,Vector{Char}}()
    all_steps = Dict{Char,Step}()

    open(filename) do file
        for line in eachline(file)
            a, b = match(r"Step (\w) must be finished before step (\w) can begin.", line).captures
            a, b = only(a), only(b)
            if !haskey(deps, b)
                deps[b] = Char[]
            end
            push!(deps[b], a)
            if !haskey(all_steps, a)
                all_steps[a] = Step(a, Int(a - 'A') + 61)
            end
            if !haskey(all_steps, b)
                all_steps[b] = Step(b, Int(b - 'A') + 61)
            end
        end
    end

    return deps, all_steps
end

function simulate_work(deps, all_steps, num_workers, base_duration)
    workers = zeros(Int, num_workers)
    tasks = fill('0', num_workers)
    time = 0

    while !isempty(all_steps)
        available = Char[]
        for (step, _) in all_steps
            if isempty(get(deps, step, [])) && !(step in tasks)
                push!(available, step)
            end
        end
        sort!(available)

        for i in 1:num_workers
            if workers[i] == 0 && !isempty(available)
                tasks[i] = popfirst!(available)
                workers[i] = all_steps[tasks[i]].duration
            end
        end

        min_duration = minimum(workers[workers .> 0])
        for i in 1:num_workers
            if workers[i] != 0
                workers[i] -= min_duration
                if workers[i] == 0
                    finish_step(deps, all_steps, tasks[i])
                    tasks[i] = '0'
                end
            end
        end
        time += min_duration
    end

    return time
end

function finish_step(deps, all_steps, step)
    delete!(all_steps, step)
    for s in keys(all_steps)
        if haskey(deps, s)
            filter!(x -> x != step, deps[s])
        end
    end
end

function main()
    deps, all_steps = parse_input("input.txt")
    time_taken = simulate_work(deps, all_steps, 5, 60)
    println(time_taken)
end

main()